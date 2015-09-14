#lang racket/base

(require 6502/assembler
         6502/object)

(provide sound-test)

(define sound-test
  (6502asm
   (% SECTION "HEADER" 0 16
      ((% STRING "NES")
       (% BYTE #x1a 2 1)))

   (% SECTION "CHARS" 0 (* 8 1024)
      ((% FILE "assets/mario.chr")))

   (% SECTION "PROGRAM" #x8000 (- (* 32 1024) 6)
      ((: "irq")
       (: "nmi")
       (: "reset")
       (lda (! 1))
       (sta #x4015)
       (lda (! #x9f))
       (sta #x4000)
       (lda (! #x22))
       (sta #x4003)

       (: "forever")
       (jmp "forever")))
   
   (% SECTION "VECTORS" #xFFFA 6
      ((% WORD "nmi" "reset" "irq")))))
