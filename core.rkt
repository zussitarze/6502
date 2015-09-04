#lang racket/base

(require "bitutils.rkt")

(require (for-syntax racket/base)
         racket/include
         racket/list
         racket/match
         racket/require
         racket/format)

(require racket/fixnum)
;; (require (filtered-in
;;           (λ (name) (regexp-replace #rx"unsafe-" name ""))
;;           racket/unsafe/ops))

(module+ test
  (require rackunit))

(provide load/execute loader execute opcode-tbl)

(struct cpu-register
  ([acc #:auto]
   [xidx #:auto]
   [yidx #:auto]
   [sp #:auto])
  #:mutable
  #:auto-value 0
  #:transparent)

(struct cpu-status
  ([s #:auto]
   [v #:auto]
   [b #:auto]
   [d #:auto]
   [i #:auto]
   [z #:auto]
   [c #:auto])
  #:mutable
  #:auto-value #f
  #:transparent)

(struct instruction (fn len amode))

(define-syntax (instruction-set stx)
  (define (mnemonic-norm m)
    (string->symbol (string-downcase (symbol->string (syntax->datum m)))))
  (define (mnemonic-instr m)
    (string->symbol
     (string-append "instr-" (string-downcase (symbol->string (syntax->datum m))))))
  (syntax-case stx ()
    [(instruction-set (mne op size amode) ...)
     (with-syntax
         ([(mne-norm ...) (map mnemonic-norm (syntax->list #'(mne ...)))]
          [(fn ...) (map mnemonic-instr (syntax->list #'(mne ...)))])
       #`(let ([opcode-tbl (make-hash)]
               [dispatch-tbl (make-vector 256 #f)])
           (begin
             (hash-set! opcode-tbl (cons 'mne-norm 'amode) op) ...
             (vector-set! dispatch-tbl op (instruction fn size 'amode)) ...
             (values opcode-tbl dispatch-tbl))))]))

(define (instr-adc pc reg st amode loc load store!)
  (let* ([acc (cpu-register-acc reg)]
         [arg (load amode loc)]
         [sum (fx+multi arg acc (if (cpu-status-c st) 1 0))]
         [res (fxand #xff sum)])
    (set-cpu-status-c! st (fx> sum #xff))
    (set-cpu-status-z! st (fx= 0 res))
    (set-cpu-status-v! st (fx= #x80 (fxand (fxand (fxxor acc res)
                                                  (fxxor arg res))
                                           #x80)))
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-register-acc! reg res)))

(define (instr-sbc pc reg st amode loc load store!)
  (let* ([acc (cpu-register-acc reg)]
         [arg (fxand #xff (fxnot (load amode loc)))]
         [sum (fx+multi arg acc (if (cpu-status-c st) 1 0))]
         [res (fxand #xff sum)])
    (set-cpu-status-c! st (fx> sum #xff))
    (set-cpu-status-z! st (fx= 0 res))
    (set-cpu-status-v! st (fx= #x80 (fxand (fxand (fxxor acc res)
                                                  (fxxor arg res))
                                           #x80)))
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-register-acc! reg res)))

(define (instr-and pc reg st amode loc load store!)
  (let* ([arg (load amode loc)]
         [res (fxand (cpu-register-acc reg) arg)])
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-status-z! st (fx= 0 res))
    (set-cpu-register-acc! reg res)))

(define (instr-asl pc reg st amode loc load store!)
  (let* ([arg (if (eq? amode 'accumulator)
                  (cpu-register-acc reg)
                  (load amode loc))]
         [res (fxand (fxlshift arg 1) #xff)])
    (set-cpu-status-c! st (bitwise-bit-set? arg 7))
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-status-z! st (fx= 0 res))
    (if (eq? amode 'accumulator)
        (set-cpu-register-acc! reg res)
        (store! amode loc res)))
  pc)

(define (offset-program-counter! pc offset)
  (let ([res (if (bitwise-bit-set? offset 7)
                 (fx- offset 256)
                 offset)])
    (set-box! pc (8bit+ res (unbox pc)))))

(define (instr-bcc pc reg st amode loc load store!)
  (unless (cpu-status-c st)
    (offset-program-counter! pc loc)))

(define (instr-bcs pc reg st amode loc load store!)
  (when (cpu-status-c st)
    (offset-program-counter! pc loc)))

(define (instr-bne pc reg st amode loc load store!)
  (unless (cpu-status-z st)
    (offset-program-counter! pc loc)))

(define (instr-beq pc reg st amode loc load store!)
  (when (cpu-status-z st)
    (offset-program-counter! pc loc)))

(define (instr-bpl pc reg st amode loc load store!)
  (unless (cpu-status-s st)
    (offset-program-counter! pc loc)))

(define (instr-bmi pc reg st amode loc load store!)
  (when (cpu-status-s st)
    (offset-program-counter! pc loc)))

(define (instr-bvc pc reg st amode loc load store!)
  (unless (cpu-status-v st)
    (offset-program-counter! pc loc)))

(define (instr-bvs pc reg st amode loc load store!)
  (when (cpu-status-v st)
    (offset-program-counter! pc loc)))

(define (instr-bit pc reg st amode loc load store!)
  (let* ([arg (load amode loc)]
         [res (fxand (cpu-register-acc reg) arg)])
    (set-cpu-status-z! (fx= res 0))
    (set-cpu-status-v! (bitwise-bit-set? arg 6))
    (set-cpu-status-s! (bitwise-bit-set? arg 7))))

(define (instr-clc pc reg st amode loc load store!)
  (set-cpu-status-c! st #f))

(define (instr-cld pc reg st amode loc load store!)
  (set-cpu-status-d! st #f))

(define (instr-cli pc reg st amode loc load store!)
  (set-cpu-status-i! st #f))

(define (instr-clv pc reg st amode loc load store!)
  (set-cpu-status-v! st #f))

(define (instr-cmp pc reg st amode loc load store!)
  (let* ([arg (load amode loc)]
         [sum (fx+ (cpu-register-acc reg)
                   (fx+ (fxand #xff (fxnot arg)) 1))]
         [res (fxand #xff sum)])
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-status-z! st (fx= res 0))
    (set-cpu-status-c! st (fx> sum #xff))))

(define (instr-cpx pc reg st amode loc load store!)
  (let* ([arg (load amode loc)]
         [sum (fx+ (cpu-register-xidx reg)
                   (fx+ (fxand #xff (fxnot arg)) 1))]
         [res (fxand #xff sum)])
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-status-z! st (fx= res 0))
    (set-cpu-status-c! st (fx> sum #xff))))

(define (instr-cpy pc reg st amode loc load store!)
  (let* ([arg (load amode loc)]
         [sum (fx+ (cpu-register-yidx reg)
                   (fx+ (fxand #xff (fxnot arg)) 1))]
         [res (fxand #xff sum)])
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-status-z! st (fx= res 0))
    (set-cpu-status-c! st (fx> sum #xff))))

(define (instr-inc pc reg st amode loc load store!)
  (let* ([arg (load amode loc)]
         [res (8bit+ arg 1)])
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-status-z! st (fx= res 0))
    (store! amode loc res)))

(define (instr-inx pc reg st amode loc load store!)
  (let* ([arg (cpu-register-xidx reg)]
         [res (8bit+ arg 1)])
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-status-z! st (fx= res 0))
    (set-cpu-register-xidx! reg res)))

(define (instr-iny pc reg st amode loc load store!)
  (let* ([arg (cpu-register-yidx reg)]
         [res (8bit+ arg 1)])
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-status-z! st (fx= res 0))
    (set-cpu-register-yidx! reg res)))

(define (instr-dec pc reg st amode loc load store!)
  (let* ([arg (load amode loc)]
         [res (8bit+ arg -1)])
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-status-z! st (fx= res 0))
    (store! amode loc res)))

(define (instr-dex pc reg st amode loc load store!)
  (let* ([arg (cpu-register-xidx reg)]
         [res (8bit+ arg -1)])
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-status-z! st (fx= res 0))
    (set-cpu-register-xidx! reg res)))

(define (instr-dey pc reg st amode loc load store!)
  (let* ([arg (cpu-register-yidx reg)]
         [res (8bit+ arg -1)])
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-status-z! st (fx= res 0))
    (set-cpu-register-yidx! reg res)))

(define (instr-eor pc reg st amode loc load store!)
  (let* ([arg (load amode loc)]
         [res (fxxor arg (cpu-register-acc reg))])
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-status-z! st (fx= res 0))
    (set-cpu-register-acc! reg res)))

(define (instr-lda pc reg st amode loc load store!)
  (let ([arg (load amode loc)])
    (set-cpu-status-s! st (bitwise-bit-set? arg 7))
    (set-cpu-status-z! st (fx= arg 0))
    (set-cpu-register-acc! reg arg)))

(define (instr-lsr pc reg st amode loc load store!)
  (let* ([arg (if (eq? amode 'accumulator)
                  (cpu-register-acc reg)
                  (load amode loc))]
         [res (fxrshift arg 1)])
    (set-cpu-status-c! st (bitwise-bit-set? arg 1))
    (set-cpu-status-s! st #f)
    (set-cpu-status-z! st (fx= res 0))
    (if (eq? amode 'accumulator)
        (set-cpu-register-acc! reg res)
        (store! amode loc res))))

(define (instr-ldx pc reg st amode loc load store!)
  (let ([arg (load amode loc)])
    (set-cpu-status-s! st (bitwise-bit-set? arg 7))
    (set-cpu-status-z! st (fx= arg 0))
    (set-cpu-register-xidx! reg arg)))

(define (instr-ldy pc reg st amode loc load store!)
  (let ([arg (load amode loc)])
    (set-cpu-status-s! st (bitwise-bit-set? arg 7))
    (set-cpu-status-z! st (fx= arg 0))
    (set-cpu-register-yidx! reg arg)))

(define (instr-nop pc reg st amode loc load store!)
  (void))

(define (instr-ora pc reg st amode loc load store!)
  (let* ([arg (load amode loc)]
         [res (fxior arg (cpu-register-acc reg))])
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-status-z! st (fx= res 0))
    (set-cpu-register-acc! reg res)))

(define (instr-pha pc reg st amode loc load store!)
  (store! 'stack #f (cpu-register-acc reg)))

(define (instr-pla pc reg st amode loc load store!)
  (let ([res (load 'stack #f)])
    (set-cpu-status-z! st (fx= res 0))
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-register-acc! reg res)))

(define (instr-php pc reg st amode loc load store!)
  (let ([res (+ (if (cpu-status-c st) 1 0)
                (if (cpu-status-z st) 2 0)
                (if (cpu-status-i st) 4 0)
                (if (cpu-status-d st) 8 0)
                (if (cpu-status-b st) 16 0)
                (if (cpu-status-v st) 64 0)
                (if (cpu-status-s st) 128 0))])
    (store! 'stack #f res)))

(define (instr-plp pc reg st amode loc load store!)
  (let ([res (load 'stack #f)])
    (set-cpu-status-c! st (bitwise-bit-set? res 0))
    (set-cpu-status-z! st (bitwise-bit-set? res 1))
    (set-cpu-status-i! st (bitwise-bit-set? res 2))
    (set-cpu-status-d! st (bitwise-bit-set? res 3))
    (set-cpu-status-b! st (bitwise-bit-set? res 4))
    (set-cpu-status-v! st (bitwise-bit-set? res 6))
    (set-cpu-status-s! st (bitwise-bit-set? res 7))))

(define (instr-brk  pc reg st amode loc load store!)
  (offset-program-counter! pc 1)
  (set-cpu-status-b! st #t)
  (store! 'stack #f (fxrshift (unbox pc) 8))
  (store! 'stack #f (fxand #xff (unbox pc)))
  (instr-php pc reg st amode loc load store!)
  (set-cpu-status-i! st #t)
  (set-box! pc (load 'indirect #xFFFE)))

(define (instr-rti pc reg st amode loc load store!)
  (instr-plp pc reg st amode loc load store!)
  (set-box! pc (16bit+ (load 'stack #f)
                       (fxlshift (load 'stack #f) 8)
                       1)))

(define (instr-rts pc reg st amode loc load store!)
  (set-box! pc (16bit+ (load 'stack #f)
                       (fxlshift (load 'stack #f) 8)
                       1)))

(define (instr-jsr pc reg st amode loc load store!)
  (let ([upc (fx- (unbox pc) 1)]) ;; point to third byte of intstr
    (store! 'stack #f (fxrshift upc 8))
    (store! 'stack #f (fxand #xff upc))
    (set-box! pc loc)))

(define (instr-jmp pc reg st amode loc load store!)
  (let ([dest (if (eq? amode 'indirect)
                  (load amode loc)
                  loc)])
    (set-box! pc dest)))

(define (instr-rol pc reg st amode loc load store!)
  (let* ([arg (if (eq? amode 'accumulator)
                  (cpu-register-acc reg)
                  (load amode loc))]
         [res (fxior (fxand #xff (fxrshift arg 1))
                     (if (cpu-status-c st)
                         128
                         0))])
    (set-cpu-status-c! st (bitwise-bit-set? arg 7))
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-status-z! st (fx= res 0))
    (if (eq? amode 'accumulator)
        (set-cpu-register-acc! reg res)
        (store! amode loc res))))

(define (instr-ror pc reg st amode loc load store!)
  (let* ([arg (if (eq? amode 'accumulator)
                  (cpu-register-acc reg)
                  (load amode loc))]
         [res (fxior (fxand #xff (fxlshift arg 1))
                     (if (cpu-status-c st)
                         1
                         0))])
    (set-cpu-status-c! st (bitwise-bit-set? arg 1))
    (set-cpu-status-s! st (bitwise-bit-set? res 7))
    (set-cpu-status-z! st (fx= res 0))
    (if (eq? amode 'accumulator)
        (set-cpu-register-acc! reg res)
        (store! amode loc res))))

(define (instr-sei pc reg st amode loc load store!)
  (set-cpu-status-i! st #t))

(define (instr-sed pc reg st amode loc load store!)
  (set-cpu-status-d! st #t))

(define (instr-sec pc reg st amode loc load store!)
  (set-cpu-status-c! st #t))

(define (instr-sta pc reg st amode loc load store!)
  (store! amode loc (cpu-register-acc reg)))

(define (instr-stx pc reg st amode loc load store!)
  (store! amode loc (cpu-register-xidx reg)))

(define (instr-sty pc reg st amode loc load store!)
  (store! amode loc (cpu-register-yidx reg)))

(define (instr-txs pc reg st amode loc load store!)
  (set-cpu-register-sp! reg (cpu-register-xidx reg)))

(define (instr-tsx pc reg st amode loc load store!)
  (define res (cpu-register-sp reg))
  (set-cpu-status-z! st (fx= res 0))
  (set-cpu-status-s! st (bitwise-bit-set? res 7))
  (set-cpu-register-xidx! reg res))

(define (instr-tax pc reg st amode loc load store!)
  (define res (cpu-register-acc reg))
  (set-cpu-status-z! st (fx= res 0))
  (set-cpu-status-s! st (bitwise-bit-set? res 7))
  (set-cpu-register-xidx! reg res))

(define (instr-txa pc reg st amode loc load store!)
  (define res (cpu-register-xidx reg))
  (set-cpu-status-z! st (fx= res 0))
  (set-cpu-status-s! st (bitwise-bit-set? res 7))
  (set-cpu-register-acc! reg res))

(define (instr-tay pc reg st amode loc load store!)
  (define res (cpu-register-acc reg))
  (set-cpu-status-z! st (fx= res 0))
  (set-cpu-status-s! st (bitwise-bit-set? res 7))
  (set-cpu-register-yidx! reg res))

(define (instr-tya pc reg st amode loc load store!)
  (define res (cpu-register-yidx reg))
  (set-cpu-status-z! st (fx= res 0))
  (set-cpu-status-s! st (bitwise-bit-set? res 7))
  (set-cpu-register-acc! reg res))

(define-values (opcode-tbl dispatch-tbl)
  (instruction-set
   (ADC #x61 2 pre-indexed-x)
   (ADC #x65 2 zero-direct)
   (ADC #x69 2 immediate)
   (ADC #x6D 3 absolute-direct)
   (ADC #x71 2 post-indexed-y)
   (ADC #x75 2 zero-idx-x)
   (ADC #x79 3 absolute-idx-y)
   (ADC #x7D 3 absolute-idx-x)
   
   (AND #x21 2 pre-indexed-x)
   (AND #x25 2 zero-direct)
   (AND #x29 2 immediate)
   (AND #x2D 3 absolute-direct)
   (AND #x31 2 post-indexed-y)
   (AND #x35 2 zero-idx-x)
   (AND #x39 3 absolute-idx-y)
   (AND #x3D 3 absolute-idx-x)
   
   (ASL #x0A 1 accumulator)
   (ASL #x06 2 zero-direct)
   (ASL #x0E 3 absolute-direct)
   (ASL #x16 2 zero-idx-x)
   (ASL #x1E 3 absolute-idx-x)
   
   (BCC #x90 2 relative)
   (BCS #xB0 2 relative)
   (BEQ #xF0 2 relative)
   (BMI #x30 2 relative)
   (BNE #xD0 2 relative)
   (BPL #x10 2 relative)
   (BVC #x50 2 relative)
   (BVS #x70 2 relative)
   
   (BIT #x24 2 zero-direct)
   (BIT #x2C 3 absolute-direct)
   
   (BRK #x00 1 implied)
   
   (CLC #x18 1 implied)
   (CLD #xD8 1 implied)
   (CLI #x58 1 implied)
   (CLV #xB8 1 implied)
   
   (CMP #xC1 2 pre-indexed-x)
   (CMP #xC5 2 zero-direct)
   (CMP #xC9 2 immediate)
   (CMP #xCD 3 absolute-direct)
   (CMP #xD1 2 post-indexed-y)
   (CMP #xD5 2 zero-idx-x)
   (CMP #xD9 3 absolute-idx-y)
   (CMP #xDD 3 absolute-idx-x)
   
   (CPX #xE0 2 immediate)
   (CPX #xE4 2 zero-direct)
   (CPX #xEC 3 absolute-direct)
   
   (CPY #xC0 2 immediate)
   (CPY #xC4 2 zero-direct)
   (CPY #xCC 3 absolute-direct)
   
   (DEC #xC6 2 zero-direct)
   (DEC #xCE 3 absolute-direct)
   (DEC #xD6 2 zero-idx-x)
   (DEC #xDE 3 absolute-idx-x)
   
   (DEX #xCA 1 implied)
   (DEY #x88 1 implied)
   
   (EOR #x41 2 pre-indexed-x)
   (EOR #x45 2 zero-direct)
   (EOR #x49 2 immediate)
   (EOR #x4D 3 absolute-direct)
   (EOR #x51 2 post-indexed-y)
   (EOR #x55 2 zero-idx-x)
   (EOR #x59 3 absolute-idx-y)
   (EOR #x5D 3 absolute-idx-x)
   
   (INC #xE6 2 zero-direct)
   (INC #xEE 3 absolute-direct)
   (INC #xF6 2 zero-idx-x)
   (INC #xFE 3 absolute-idx-x)
   
   (INX #xE8 1 implied)
   (INY #xC8 1 implied)
   
   (JMP #x4C 3 absolute-direct)
   (JMP #x6C 3 indirect)
   
   (JSR #x20 3 absolute-direct)
   
   (LDA #xA1 2 pre-indexed-x)
   (LDA #xA5 2 zero-direct)
   (LDA #xA9 2 immediate)
   (LDA #xAD 3 absolute-direct)
   (LDA #xB1 2 post-indexed-y)
   (LDA #xB5 2 zero-idx-x)
   (LDA #xB9 3 absolute-idx-y)
   (LDA #xBD 3 absolute-idx-x)
   
   (LDX #xA2 2 immediate)
   (LDX #xA6 2 zero-direct)
   (LDX #xAE 3 absolute-direct)
   (LDX #xB6 2 zero-idx-y)
   (LDX #xBE 3 absolute-idx-y)
   
   (LDY #xA0 2 immediate)
   (LDY #xA4 2 zero-direct)
   (LDY #xAC 3 absolute-direct)
   (LDY #xB4 2 zero-idx-x)
   (LDY #xBC 3 absolute-idx-x)
   
   (LSR #x4a 1 accumulator)
   (LSR #x46 2 zero-direct)
   (LSR #x4E 3 absolute-direct)
   (LSR #x56 2 zero-idx-x)
   (LSR #x5E 3 absolute-idx-x)
   
   (NOP #xEA 1 implied)
   
   (ORA #x01 2 pre-indexed-x)
   (ORA #x05 2 zero-direct)
   (ORA #x09 2 immediate)
   (ORA #x0D 3 absolute-direct)
   (ORA #x11 2 post-indexed-y)
   (ORA #x15 2 zero-idx-x)
   (ORA #x19 3 absolute-idx-y)
   (ORA #x1D 3 absolute-idx-x)
   
   (PHA #x48 1 implied)
   (PLA #x68 1 implied)
   (PHP #x08 1 implied)
   (PLP #x28 1 implied)
   
   (ROL #x2A 1 accumulator)
   (ROL #x26 2 zero-direct)
   (ROL #x2E 3 absolute-direct)
   (ROL #x36 2 zero-idx-x)
   (ROL #x3E 3 absolute-idx-x)
   
   (ROR #x6A 1 accumulator)
   (ROR #x66 2 zero-direct)
   (ROR #x6E 3 absolute-direct)
   (ROR #x76 2 zero-idx-x)
   (ROR #x7E 3 absolute-idx-x)
   
   (RTI #x40 1 implied)
   (RTS #x60 1 implied)
   
   (SBC #xE1 2 pre-indexed-x)
   (SBC #xE5 2 zero-direct)
   (SBC #xE9 2 immediate)
   (SBC #xED 3 absolute-direct)
   (SBC #xF1 2 post-indexed-y)
   (SBC #xF5 2 zero-idx-x)
   (SBC #xF9 3 absolute-idx-y)
   (SBC #xFD 3 absolute-idx-x)
   
   (SEI #x78 1 implied)
   (SED #xF8 1 implied)
   (SEC #x38 1 implied)
   
   (STA #x81 2 pre-indexed-x)
   (STA #x85 2 zero-direct)
   (STA #x8D 3 absolute-direct)
   (STA #x91 2 post-indexed-y)
   (STA #x95 2 zero-idx-x)
   (STA #x99 3 absolute-idx-y)
   (STA #x9D 3 absolute-idx-x)
   
   (STX #x86 2 zero-direct)
   (STX #x8E 3 absolute-direct)
   (STX #x96 2 zero-idx-y)
   
   (STY #x84 2 zero-direct)
   (STY #x8C 3 absolute-direct)
   (STY #x94 2 zero-idx-x)
   
   (TAX #xAA 1 implied)
   (TXA #x8A 1 implied)
   
   (TAY #xA8 1 implied)
   (TYA #x98 1 implied)
   
   (TSX #xBA 1 implied)
   (TXS #x9A 1 implied)
   ))

(module+ test
  (check-eq? (hash-count opcode-tbl) 151))

(define (load/execute obj)
  (execute (loader obj) (caar obj)))

(define (loader obj
                #:memory-size [memory-size (* 64 1024)]
                #:use-ext-memory [extmem #f])
  (define memory (or extmem (make-bytes memory-size 0)))
  (for/fold ([prev (caar obj)])
            ([seg (in-list obj)])
    (when (< (car seg) prev)
      (error "Loader error: Object file contains overlapping segment at" (car seg)))
    (bytes-copy! memory (car seg) (cdr seg))
    (+ prev (bytes-length (cdr seg))))
  memory)

(define (execute memory initpc)
  (define bpc (box initpc))
  (define register (cpu-register))
  (define status (cpu-status))
  
  (define (load amode loc)
    (case amode
      [(immediate) loc]
      [(stack)
       (let ([sp (8bit+ (cpu-register-sp register) 1)])
         (set-cpu-register-sp! register sp)
         (bytes-ref memory (fx+ #x0100 sp)))]
      [(zero-direct absolute-direct)
       (bytes-ref memory loc)]
      [(zero-idx-x)
       (bytes-ref memory (8bit+ loc (cpu-register-xidx register)))]
      [(absolute-idx-x)
       (bytes-ref memory (16bit+ loc (cpu-register-xidx register)))]
      [(zero-idx-y)
       (bytes-ref memory (8bit+ loc (cpu-register-yidx register)))]
      [(absolute-idx-y)
       (bytes-ref memory (16bit+ loc (cpu-register-yidx register)))]      
      [(indirect)
       (bytes-ref memory (16bit+ (bytes-ref memory loc)
                                 (fxlshift (bytes-ref memory (8bit+ loc 1)) 8)))]
      [(pre-indexed-x)
       (let* ([iaddr (8bit+ loc (cpu-register-xidx register))]
              [addr (16bit+ (bytes-ref memory iaddr)
                            (fxlshift (bytes-ref memory (8bit+ iaddr 1)) 8))])
         (bytes-ref memory addr))]
      [(post-indexed-y)
       (let* ([base (16bit+ (bytes-ref memory loc)
                            (fxlshift (bytes-ref memory (8bit+ loc 1)) 8))]
              [addr (16bit+ base (cpu-register-yidx register))])
         (bytes-ref memory addr))]
      [else
       (error "Unsupported memory load with mode" amode)]))
  
  (define (store! amode loc val)
    (case amode
      [(stack)
       (let ([sp (cpu-register-sp register)])
         (bytes-set! memory (fx+ #x0100 sp) val)
         (set-cpu-register-sp! register (fxand #xff (fx- sp 1))))]
      [(zero-direct absolute-direct)
       (bytes-set! memory loc val)]
      [(zero-idx-x)
       (bytes-set! memory (8bit+ loc (cpu-register-xidx register)) val)]
      [(absolute-idx-x)
       (bytes-set! memory (16bit+ loc (cpu-register-xidx register)) val)]
      [(zero-idx-y)
       (bytes-set! memory (8bit+ loc (cpu-register-yidx register)) val)]
      [(absolute-idx-y)
       (bytes-set! memory (16bit+ loc (cpu-register-yidx register)) val)]
      [(pre-indexed-x)
       (let* ([iaddr (8bit+ loc (cpu-register-xidx register))]
              [addr (16bit+ (bytes-ref memory iaddr)
                            (fxlshift (bytes-ref memory (8bit+ iaddr 1)) 8))])
         (bytes-set! memory addr val))]
      [(post-indexed-y)
       (let* ([base (16bit+ (bytes-ref memory loc)
                            (fxlshift (bytes-ref memory (8bit+ loc 1)) 8))]
              [addr (16bit+ base (cpu-register-yidx register))])
         (bytes-set! memory addr val))]
      [else (error "Unsupported memory store with mode")]))
  
  (define (runloop [counter 0])
    (if (cpu-status-b status)
        counter
        (let* ([pc (unbox bpc)]
               [i (vector-ref dispatch-tbl (bytes-ref memory pc))]
               [loc (case (instruction-len i)
                      [(1) #f]
                      [(2) (bytes-ref memory (fx+ pc 1))]
                      [(3) (fx+ (bytes-ref memory (fx+ pc 1))
                                (fxlshift (bytes-ref memory (fx+ pc 2)) 8))])])
          (set-box! bpc (fx+ pc (instruction-len i)))
          ((instruction-fn i) bpc register status (instruction-amode i) loc load store!)
          (runloop (fx+ counter 1)))))
  (values (runloop) register status memory))
