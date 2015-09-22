#lang racket/base

(require "assembler.rkt"
         "bitutils.rkt"
         "core.rkt"
         "object.rkt"
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
  (6502asm
   (: "Reset")
   (LDX (! #xff))
   (TXS)
   ))

(define spinloop
  (6502asm
   (: "loop")
   (adc (! 1))
   (jmp "loop")))

(define slow-loops
  (6502asm
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

(define section-jump
  (6502asm
   (% SECTION "header" 2 10
      ((% ORG 8)
       (lda (! 1))
       (lda (! 2))))

   (lda (! 20))

   (% ORG 0)
   (lda (! 243))
   
   (% ORG 30)
   (sec)
   (: "Loop")
   (nop)
   (bcc "Terminate")
   (clc)
   (jmp "Loop")
   (nop)
   (nop)

   (% SECTION "Terminate" 50 10
      ((lda (! #x33))
       (sta #x99)
       (brk)))
   ))

(dumpobject (assemble section-jump))

;(asmexec/output section-jump)

