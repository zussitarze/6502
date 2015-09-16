#lang racket/base

(require racket/match
         racket/port
         (only-in "core.rkt" opcode-tbl)
         "object.rkt"
         "asm-macros.rkt")

(provide 6502asm
         assemble
         (struct-out section))

(define in-named-section? (make-parameter #f))

;; Assembles the source instructions into an object format
(define (assemble source)
  (define-values (sections symtable deferred)
    (parameterize ([in-named-section? #f])
      (for/fold ([sections (list (new-section 0))]
                 [symtable (hash)]
                 [deferred '()])
                ([line source])
        (asmline line sections symtable deferred))))  
  ;; Convert section streams to bytes
  (define bin-sections
    (map (lambda (s)
           (struct-copy section s [seg (get-output-bytes (section-seg s))]))
         sections))
  ;; Patch in deferred label targets
  (for ([defer (in-list deferred)])
    (match-let*
        ([(list amode name width secstart pos) defer]
         [(struct* section ([seg seg])) (findf (λ (s) (eqv? (section-start s) secstart))
                                               bin-sections)])
      (cond
        [(eq? 'relative amode)
         ;; Relative branches are always 8 bit, offsets are counted
         ;; from after branch arguments
         (let* ([addr (get-symbol symtable name (badlabel name))]
                [offset (- addr ( + secstart pos) 1)])
           (if (or (> offset 127) (< offset -128))
               (error "Branch offset too large to" name)
               (bytes-set! seg pos (if (< offset 0) (+ 256 offset) offset))))]
        [else 
         (let ([val (get-symbol symtable name (badlabel name))])
           (case width
             [(8)
              (bytes-set! seg pos val)]
             [(16)
              (bytes-set! seg pos (bitwise-and #xff val))
              (bytes-set! seg (+ pos 1) (arithmetic-shift val -8))]))])))  
  (sort (filter (λ (s)
                  (not (zero? (bytes-length (section-seg s)))))
                bin-sections)
        <
        #:key section-start))

(define (asmline line sections symtable deferred)
  (match line
    [(list 'label l)
     (values sections
             (add-symbol symtable l (+ (startar sections) (file-position (segar sections))))
             deferred)]
    [(list 'equ n v)
     (if (and (string? n) (integer? v))
         (values sections (add-symbol symtable n v) deferred)
         (error "Binding must be contain a string name and integer value"))]     
    [(list 'data-bytes bs ...)
     (for-each
      (λ (b)
        (let ([b (if (string? b)
                     (get-symbol symtable b)
                     b)])
          (write-byte b (segar sections))))
      bs)
     (values sections symtable deferred)]
    [(list 'data-words ws ...)
     (for-each
      (λ (w)
        (let ([w (if (string? w)
                     (get-symbol symtable w)
                     w)])
          (write-bytes (integer->integer-bytes w 2 #f #f) (segar sections))))
      ws)
     (values sections symtable deferred)]
    [(list 'data-string s)
     (for ([b (in-bytes (if (bytes? s) s (string->bytes/latin-1 s)))])
       (write-byte b (segar sections)))
     (values sections symtable deferred)]
    [(list 'file path)
     (call-with-input-file path
       (λ (in)
         (copy-port in (segar sections))))
     (values sections symtable deferred)]     
    [(list 'section n s l ops)
     (if (in-named-section?)
         (error "Sections cannot be nested:" n)
         (let*-values
             ([(secs2 sym2 def2)
               (parameterize ([in-named-section? #t])
                 (for/fold ([sections (cons (new-section s n) sections)]
                            [symtable (add-symbol symtable n s)]
                            [deferred deferred])
                           ([o (in-list ops)])
                   (asmline o sections symtable deferred)))])
           (cond [(<= (file-position (segar secs2)) l)
                  (file-position (segar secs2) l)
                  (values (cons (new-section (+ s l)) secs2) sym2 def2)]
                 [else (error "Section code overflows its specified size:" n l)])))]
    [(list 'origin o)
     ;; If we are inside a section, advance the location counter
     ;; taking into account the section's inherent offset.  Otherwise,
     ;; create a fresh section at o.
     (cond [(in-named-section?)
            (when (< o (+ (startar sections) (file-position (segar sections))))
              (error "ORG cannot move backwards inside a segment" o))
            (file-position (segar sections) (- o (startar sections)))
            (values sections symtable deferred)]
           [else
            (values (cons (new-section o) sections)
                    symtable
                    deferred)])]
     [(list 'operation mnemonic stx-amode operand)
      ;; Resolve label references if possible, otherwise defer writing
      ;; their value. Note that non relative-mode future references
      ;; must be assumed as being 16 bits, since their ultimate value
      ;; depends on the size of the current variable length
      ;; instruction.
     (let* ([val (cond [(memq stx-amode '(stx-implied stx-accumulator)) 'none]
                       [(or (eq? stx-amode 'stx-relative) (integer? operand)) operand]
                       [(string? operand) (get-symbol symtable operand operand)]
                       [else (error "Cannot resolve operand:" operand)])]
            [width (cond [(eq? val 'none) 0]
                         [(memq mnemonic '(jmp jsr)) 16]
                         [(eq? stx-amode 'stx-relative) 8]
                         [(and (integer? val) (>= val -128) (<= val 255)) 8]
                         [else 16])]
            [amode (case stx-amode
                     [(stx-implied) 'implied]
                     [(stx-accumulator) 'accumulator]
                     [(stx-relative) 'relative]
                     [(stx-indirect) 'indirect]
                     [(stx-immediate)
                      (assert-8bit width val)
                      'immediate]
                     [(stx-pre-indexed-x)
                      (assert-8bit width val)
                      'pre-indexed-x]
                     [(stx-post-indexed-y)
                      (assert-8bit width val)
                      'post-indexed-y]
                     [(stx-idx-x)
                      (if (= width 16) 'absolute-idx-x 'zero-idx-x)]
                     [(stx-idx-y)
                      (if (= width 16) 'absolute-idx-y 'zero-idx-y)]
                     [(stx-direct)
                      (if (= width 16) 'absolute-direct 'zero-direct)]
                     [else
                      (error "Invalid address mode stx-amode")])]
            [opcode (hash-ref opcode-tbl (cons mnemonic amode))])
       (cond
         ;; If val is a string at this point, it is a future reference
         ;; or relative mode branch.
         [(string? val)
          (let ([argpos (+ (file-position (segar sections)) 1)])
            (write-machinecode opcode 0 width (segar sections))
            (values sections
                    symtable
                    (cons (list amode val width (startar sections) argpos) deferred)))]
         [else
          (write-machinecode opcode val width (segar sections))
          (values sections symtable deferred)]))]))

(define (write-machinecode opcode operand argwidth stream)
  (write-byte opcode stream)
  (case argwidth
    [(0) (void)]
    [(8) (write-byte (if (< operand 0) (+ 256 operand) operand) stream)]
    [(16) (write-bytes (integer->integer-bytes operand 2 #f #f) stream)]
    [else (error "Bad argument width")]))    

(define (assert-8bit w v)
  (unless (= w 8)
    (error "Value must fit inside an 8bit range" v)))

(define (new-section start [name ""])
  (section name start (open-output-bytes)))

(define (segar sections)
  (section-seg (car sections)))

(define (startar sections)
  (section-start (car sections)))

(define (add-symbol st s v)
  (unless (string? s)
    (error "Symbol name must be a string" s))
  (let ([s (string-downcase s)])
    (if (hash-has-key? st s)
        (error "Symbol already bound to existing value" s (hash-ref st s))
        (hash-set st s v))))

(define (get-symbol st s [fail #f])
  (hash-ref st (string-downcase s) fail))

(define (badlabel l)
  (lambda ()
    (error "Label not found" l)))
