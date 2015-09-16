#lang racket/base

(require 6502/assembler
         6502/object)

(provide screen-test
         sprite-test
         controller-test)

;;; Examples from the "Nerdy Nights" set of tutorials.

(define screen-test
  (6502asm
   (% SECTION "HEADER" 0 16
      ((% STRING "NES")
       (% BYTE #x1a 2 1)))

   (% SECTION "CHARS" 0 (* 8 1024)
      ((% FILE "examples/mario.chr")))

   (% SECTION "PROGRAM" #x8000 (- (* 32 1024) 6)
      ((: "reset")
       (sei)          ; disable IRQs
       (cld)          ; disable decimal mode
       (ldx (! #x40))
       (stx #x4017)    ; disable APU frame IRQ
       (ldx (! #xff))
       (txs)          ; Set up stack
       (inx)          ; now X = 0
       (stx #x2000)    ; disable NMI
       (stx #x2001)    ; disable rendering
       (stx #x4010)    ; disable DMC IRQs

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
   
   (% SECTION "VECTORS" #xfffa 6
      ((% WORD "nmi" "reset" 0)))
   ))

(define sprite-test
  (6502asm
   (% SECTION "HEADER" 0 16
      ((% STRING "NES")
       (% BYTE #x1a 1 1 1)))

   (% SECTION "CHARS" 0 (* 8 1024)
      ((% FILE "examples/mario.chr")))

   (% SECTION "PROGRAM" #xC000 (- (* 16 1024) 6)
      ((: "reset")
       (sei)          ; disable irqs
       (cld)          ; disable decimal mode
       (ldx (! #x40))
       (stx #x4017)    ; disable apu frame irq
       (ldx (! #xff))
       (txs)           ; set up stack
       (inx)           ; now x = 0
       (stx #x2000)    ; disable nmi
       (stx #x2001)    ; disable rendering
       (stx #x4010)    ; disable dmc irqs

       (: "vblankwait1")
       (bit #x2002)
       (bpl "vblankwait1")

       (: "clrmem")
       (lda (! 0))
       (sta #x0000 X)
       (sta #x0100 X)
       (sta #x0300 X)
       (sta #x0400 X)
       (sta #x0500 X)
       (sta #x0600 X)
       (sta #x0700 X)
       (lda (! #xfe))
       (sta #x0200 X)
       (inx)
       (bne "clrmem")

       (: "vblankwait2")
       (bit #x2002)
       (bpl "vblankwait2")

       (: "LoadPalettes")
       (lda #x2002)
       (lda (! #x3f))
       (sta #x2006)
       (lda (! 0))
       (sta #x2006)
       (ldx (! 0))
       (: "LoadPallettesLoop")
       (lda "palette" X)
       (sta #x2007) ; writes to the PPU
       (inx)
       (cpx (! 32)) ; copy all 32 bytes.
       (bne "LoadPallettesLoop")

       (lda (! #x80))
       (sta #x0200) ; put sprite 0 in vertical center
       (sta #x0203) ; put sprite 0 in horizontal center
       (lda (! 0))
       (sta #x0201) ; tile number = 0
       (sta #x0202) ; color = 0, no flip

       (lda (! #b10000000)) ; enable NMI, pattern tbl 0 sprites
       (sta #x2000)

       (lda (! #b00010000)) ;; enable sprites
       (sta #x2001)

       (: "forever")
       (jmp "forever")

       (: "nmi")
       (lda (! 0))
       (sta #x2003) ; set low byte of RAM addr
       (lda (! 2))
       (sta #x4014) ; set high byte of RAM addr       
       (rti)

       (% ORG #xe000)
       (: "palette")
       (% BYTE #x0f #x31 #x32 #x33 #x0f #x35 #x36 #x37 #x0f #x39 #x3a #x3b #x0f #x3d #x3e #x0f)
       (% BYTE #x0f #x1c #x15 #x14 #x0f #x02 #x38 #x3c #x0f #x1c #x15 #x14 #x0f #x02 #x38 #x3c)
       ))
   
   
   (% SECTION "VECTORS" #xfffa 6
      ((% WORD "nmi" "reset" 0)))
   ))


;; Moves mario around with the D-Pad
(define controller-test
  (6502asm
   (% SECTION "HEADER" 0 16
      ((% STRING "NES")
       (% BYTE #x1a 1 1 1)))

   (% SECTION "CHARS" 0 (* 8 1024)
      ((% FILE "examples/mario.chr")))

   (% SECTION "PROGRAM" #xC000 (- (* 16 1024) 6)
      ((: "reset")
       (sei)          ; disable irqs
       (cld)          ; disable decimal mode
       (ldx (! #x40))
       (stx #x4017)    ; disable apu frame irq
       (ldx (! #xff))
       (txs)           ; set up stack
       (inx)           ; now x = 0
       (stx #x2000)    ; disable nmi
       (stx #x2001)    ; disable rendering
       (stx #x4010)    ; disable dmc irqs

       (: "vblankwait1")
       (bit #x2002)
       (bpl "vblankwait1")

       (: "clrmem")
       (lda (! 0))
       (sta #x0000 X)
       (sta #x0100 X)
       (sta #x0200 X)
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

       (: "LoadPalettes")
       (lda #x2002)
       (lda (! #x3f))
       (sta #x2006)
       (lda (! 0))
       (sta #x2006)
       (ldx (! 0))
       (: "LoadPallettesLoop")
       (lda "palette" X)
       (sta #x2007) ; writes to the PPU
       (inx)
       (cpx (! 32)) ; copy all 32 bytes.
       (bne "LoadPallettesLoop")

       (: "LoadSprites")
       (ldx (! 0))
       (: "LoadSpritesLoop")
       (lda "sprites" X)
       (sta #x0200 X)
       (inx)
       (cpx (! 32))
       (bne "LoadSpritesLoop")
            
       (lda (! #b10000000)) ; enable NMI, pattern tbl 0 sprites
       (sta #x2000)

       (lda (! #b00010000)) ;; enable sprites
       (sta #x2001)

       (: "forever")
       (jmp "forever")

       (: "nmi")
       (lda (! 0))
       (sta #x2003) ; set low byte of RAM addr
       (lda (! 2))
       (sta #x4014) ; set high byte of RAM addr       

       (: "LatchController")
       (lda (! 1))
       (sta #x4016)
       (lda (! 0))
       (sta #x4016) ; tell both controllers latch buttons

       (lda #x4016) ;; Read A
       (lda #x4016) ;; Read B
       (lda #x4016) ;; Read Select
       (lda #x4016) ;; Read Start

       (% FOR ([delta '(-1 1 -1 1)]
               [offset '(0 0 3 3)])
          (% LOCAL (done loop)
             (lda #x4016)
             (and (! 1))
             (beq done)
             (ldx (! offset))
             (: loop)
             (lda #x0200 X)
             (clc)
             (adc (! delta))
             (sta #x0200 X)
             (txa)
             (clc)
             (adc (! 4))
             (tax)
             (cpx (! (+ 16 offset)))
             (bne loop)
             (: done)))
       
       (rti)
       
       (% ORG #xe000)
       (: "palette")
       (% BYTE #x0f #x31 #x32 #x33 #x0f #x35 #x36 #x37 #x0f #x39 #x3a #x3b #x0f #x3d #x3e #x0f)
       (% BYTE #x0f #x1c #x15 #x14 #x0f #x02 #x38 #x3c #x0f #x1c #x15 #x14 #x0f #x02 #x38 #x3c)

       (: "sprites")
       ;;      vert tile attr horiz
       (% BYTE #x80 #x32 #x00 #x80) ; 0 sprite
       (% BYTE #x80 #x33 #x00 #x88) ; 1
       (% BYTE #x88 #x34 #x00 #x80) ; 2
       (% BYTE #x88 #x35 #x00 #x88) ; 3
       
       ))
   
   
   (% SECTION "VECTORS" #xfffa 6
      ((% WORD "nmi" "reset" 0)))
   ))

