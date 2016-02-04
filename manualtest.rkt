#lang racket/base

(require "assembler.rkt"
         "bitutils.rkt"
         "core.rkt"
         "object.rkt"
         racket/format
         racket/match)

(define (asmexec/output source [dump-object? #f])
  (define obj (assemble source))
  (define mem (make-bytes (* 64 1024) 0))
  (load-object obj mem)
  (define ctrl-chan (make-channel))
  (define-values (results cpu real gc)
    (time-apply
     (λ ()
       (execute (bus-with-memory mem) 0 ctrl-chan #f)
       (channel-get ctrl-chan))
     '()))
  (match-let ([(list opcount register status memory) (car results)])
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
;;(asmexec/output slow-loops)

(define section-jump
  (6502asm
   (% SECTION "header" 2 10
      (% ORG 8)
      (lda (! 1))
      (lda (! 2)))

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
      (lda (! #x33))
      (sta #x99)
      (brk))
   ))
(asmexec/output section-jump)

;; (6502asm
;;  (lda #x4016) ;; Read A
;;  (lda #x4016) ;; Read B
;;  (lda #x4016) ;; Read Select
;;  (lda #x4016) ;; Read Start

;;  (% FOR ([delta '(-1 1 -1 1)]
;;          [offset '(0 0 3 3)])
;;     (% LOCAL (done loop)
;;        (lda #x4016)
;;        (and (! 1))
;;        (beq done)
;;        (ldx (! offset))
;;        (: loop)
;;        (lda #x0200 X)
;;        (clc)
;;        (adc (! delta))
;;        (sta #x0200 X)
;;        (txa)
;;        (clc)
;;        (adc (! 4))
;;        (tax)
;;        (cpx (! (+ 16 offset)))
;;        (bne loop)
;;        (: done)))

;;  (rti))
