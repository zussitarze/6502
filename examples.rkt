#lang racket/base

(require "assembler.rkt"
         "bitutils.rkt"
         "core.rkt"
         racket/format
         racket/match)

(define (asmexec/output source [dump-object? #f])
  (define obj (assemble source))
  (define-values (results cpu real gc) (time-apply load/execute (list obj)))
  (match-let ([(list opcount register status memory) results])
    (let ([clock (if (zero? real)
                     -1
                     (/ (* opcount 4) (* real 1000)))])
      (values (when dump-object?
                (dumpobject obj))
              (format "ops: ~a ms: ~a clock: ~~ ~a MHz" opcount real (~r clock #:precision 3))
              (dumpbytes (subbytes memory #x0000 #x0100) #f)
              (dumpbytes (subbytes memory #x0100 #x0200) #t)
              (dumpbytes (subbytes memory #x0200 #x0300) #t)
              register status))))

(define asm-init
  (asm
   (: "Reset")
   (LDX (! #xff))
   (TXS)
   ))

(define slow-loops
  (asm
   (% INCLUDE asm-init)
   (LDY (! 255))
   (: "Outer")
   (LDX (! 255))
   (: "Inner")
   (CLC)
   (ADC (! 1))
   (BCC "Inner")
   (DEX)
   (BNE "Inner")
   (DEY)
   (BNE "Outer")
   (BRK)
   ))

(asmexec/output slow-loops)

