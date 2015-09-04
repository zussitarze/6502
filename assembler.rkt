#lang racket/base

(require racket/match
         (only-in "core.rkt" opcode-tbl)
         (for-syntax racket/base))

(provide asm assemble)

(define-syntax (asm stx)
  (syntax-case stx ()
    [(asm l ...)
     (with-syntax ([(pl ...) (map parse-asm-line (syntax->list #'(l ...)))])
       #'(list pl ...))]))

(define-for-syntax (parse-asm-line stx)
  (define (normalize-sym s)
    (string->symbol (string-downcase (symbol->string (syntax->datum s)))))
  (syntax-case stx (: % EQU ORG INCLUDE BYTE STRING INCLUDE)
    [(% EQU n v) #'(list 'equ n v)] 
    [(% ORG l) #'(list 'origin l)]
    [(% BYTE bs ...) #'(list 'data-bytes bs ...)]
    [(% STRING s) #'(list 'data-string s)]
    [(% INCLUDE i) #'(list 'include i)]
    [(% other ...)
     (raise-syntax-error #f "Invalid pseudo instruction" stx)]
    [(: l) #'(list 'label l)]
    [(mnemonic arg ...)
     (with-syntax
       ([(amode operand)
        (syntax-case #'(arg ...) (! @ ^ X Y A)
          [((! v)) #'('stx-immediate v)]
          [((@ v X)) #'('stx-pre-indexed-x v)]
          [((@ v) Y) #'('stx-post-indexed-y v)]
          [((@ v)) #'('stx-indirect v)]
          [(A) #'('stx-accumulator 'none)]
          [(v X) #'('stx-idx-x v)]
          [(v Y) #'('stx-idx-y v)]
          [(v) (if (memq (normalize-sym #'mnemonic) '(bcc bcs beq bne bmi bpl bvc bvs))
                   #'('stx-relative v)
                   #'('stx-direct v))]
          [() #'('stx-implied 0)])])
       #`(list 'operation '#,(normalize-sym #'mnemonic) amode operand))]))

;; Assembles the source instructions into an object format
(define (assemble source)
  (define-values (segments symtable deferred)
    (for/fold ([segments (list (cons 0 (open-output-bytes)))]
               [symtable (hash)]
               [deferred '()])
              ([line source])
      (asmline line segments symtable deferred)))
  ;; Convert segments from streams to bytes
  (define bin-segments
    (map (lambda (seg)
           (cons (car seg) (get-output-bytes (cdr seg))))
         segments))
  ;; Patch in deferred label targets
  (for ([defer (in-list deferred)])
    (match-let* ([(list amode name width segstart pos) defer]
                 [(cons _ seg) (assoc segstart bin-segments)])
      (cond
        [(eq? 'relative amode)
         ;; Relative branches are always 8 bit, offsets are counted
         ;; from after branch arguments
         (let* ([addr (hash-ref symtable name (badlabel name))]
                [offset (- addr ( + segstart pos) 1)])
           (if (or (> offset 127) (< offset -128))
               (error "Branch offset too large to" name)
               (bytes-set! seg pos (if (< offset 0) (+ 256 offset) offset))))]
        [else 
         (let ([val (hash-ref symtable name (badlabel name))])
           (case width
             [(8)
              (bytes-set! seg pos val)]
             [(16)
              (bytes-set! seg pos (bitwise-and #xff val))
              (bytes-set! seg (+ pos 1) (arithmetic-shift val -8))]))])))
  (sort (filter (λ (s)
                  (not (zero? (bytes-length (cdr s)))))
                bin-segments)
        <
        #:key car))

(define (asmline line segments symtable deferred)
    (match line
      [(list 'label l)
       (unless (string? l)
         (error "Label must be a string value" l))
       (when (hash-has-key? symtable l)
         (error "Label cannot be redefined" l))       
       (values segments
               (hash-set symtable l (+ (caar segments) (file-position (cdar segments))))
               deferred)]
      [(list 'equ n v)
       (unless (and (string? n) (integer? v))
         (error "Binding must be contain a string name and integer value"))
       (when (hash-has-key? symtable n)
         (error "Binding already in use" n))
       (values segments
               (hash-set symtable n v)
               deferred)]
      [(list 'data-bytes bs ...)
       (for-each (lambda (b) (write-byte b (cdar segments))) bs)
       (values segments symtable deferred)]
      [(list 'data-string s)
       (for ([b (in-bytes (if (bytes? s) s (string->bytes/latin-1 s)))])
         (write-byte b (cdar segments)))
       (values segments symtable deferred)]
      [(list 'include included)
       (for/fold ([segments segments]
                  [symtable symtable]
                  [deferred deferred])                 
                 ([l (in-list included)])
         (asmline l segments symtable deferred))]
      [(list 'origin o)
       (values (cons (cons o (open-output-bytes)) segments)
               symtable
               deferred)]      
      [(list 'operation mnemonic stx-amode operand)
       ;; Resolve label references if possible, otherwise defer
       ;; writing their value. Note that non relative-mode future
       ;; references must be assumed as being 16 bits, since their
       ;; ultimate value depends on the size of the current variable
       ;; length instruction.
       (let* ([val (cond [(memq stx-amode '(stx-implied stx-accumulator)) 'none]
                         [(or (eq? stx-amode 'stx-relative) (integer? operand)) operand]
                         [(string? operand) (hash-ref symtable operand operand)]
                         [else (error "Cannot resolve operand" operand)])]
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
           ;; If val is a string at this point, it is a future
           ;; reference or relative mode branch.
           [(string? val)
            (let ([argpos (+ (file-position (cdar segments)) 1)])
              (write-machinecode opcode 0 width (cdar segments))
              (values segments
                      symtable
                      (cons (list amode val width (caar segments) argpos) deferred)))]
           [else
            (write-machinecode opcode val width (cdar segments))
            (values segments symtable deferred)]))]))

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

(define (badlabel l)
  (lambda ()
    (error "Label not found" l)))
