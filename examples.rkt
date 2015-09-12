#lang racket/base

(require 6502/assembler
         6502/object)

(provide sound-test screen-test)

;;; A collection of example programs for testing NES development.

(define screen-test
  (6502asm
   (% SECTION "HEADER" 0 16
      ((% STRING "NES")
       (% BYTE #x1a 2 1)))

   (% SECTION "CHARS" 0 (* 8 1024)
      (
       #;(% FILE "assets/mario.chr")))

   (% SECTION "PROGRAM" #x8000 (- (* 32 1024) 6)
      ((: "reset")
       (SEI)          ; disable IRQs
       (CLD)          ; disable decimal mode
       (LDX (! #x40))
       (STX #x4017)    ; disable APU frame IRQ
       (LDX (! #xFF))
       (TXS)          ; Set up stack
       (INX)          ; now X = 0
       (STX #x2000)    ; disable NMI
       (STX #x2001)    ; disable rendering
       (STX #x4010)    ; disable DMC IRQs

       (: "vblankwait1")
       (bit #x2002)
       (bpl "vblankwait1")

       (: "clrmem")
       (lda (! 0))
       (sta #x0000 X)
       (sta #x0100 X)
       (sta #x0200 X)
       (sta #x0300 X)
       (sta #x0400 X)
       (sta #x0500 X)
       (sta #x0600 X)
       (sta #x0700 X)
       (lda (! #xfe))
       (sta #x0300 X)
       (inx)
       (bne "clrmem")

       (: "vblankwait2")
       (bit #x2002)
       (bpl "vblankwait2")

       (lda (! #b01000000))
       (sta #x2001)

       (: "forever")
       (jmp "forever")

       (: "nmi")
       (rti)))
   
   (% SECTION "VECTORS" #xFFFA 6
      ((% WORD "nmi" "reset" 0)))))


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
