#lang racket/base

(require "../../assembler.rkt"
         "../../object.rkt")

(provide background-test)

;;; Examples from the "Nerdy Nights" set of tutorials.

(define background-test
  (6502asm
   (% SECTION "HEADER" 0 16
      (% STRING "NES")
      (% BYTE #x1a 1 1 1))

   (% SECTION "CHARS" 0 (* 8 1024)
      (% FILE "examples/mario.chr"))

   (% SECTION "PROGRAM" #xC000 (- (* 16 1024) 6)
      (: "reset")
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
      (cpx (! #x20)) ; copy all 32 bytes.
      (bne "LoadPallettesLoop")

      (: "LoadSprites")
      (ldx (! 0))
      (: "LoadSpritesLoop")
      (lda "sprites" X)
      (sta #x0200 X)
      (inx)
      (cpx (! #x10))
      (bne "LoadSpritesLoop")

      (: "LoadBackground")
      (lda #x2002) ; read PPU status to reset high/low latch
      (lda (! #x20))
      (sta #x2006)
      (lda (! 0))
      (sta #x2006)
      (ldx (! 0))
      (: "LoadBackgroundLoop")
      (lda "background" X)
      (sta #x2007)
      (inx)
      (cpx (! #x80))
      (bne "LoadBackgroundLoop")

      (: "LoadAttribute")
      (lda #x2002) ; read PPU status to reset high/low latch
      (lda (! #x23))
      (sta #x2006)
      (lda (! #xc0))
      (sta #x2006)
      (ldx (! 0))
      (: "LoadAttributeLoop")
      (lda "attribute" X)
      (sta #x2007)
      (inx)
      (cpx (! 8))
      (bne "LoadAttributeLoop")

      (lda (! #b10010000)) ; enable NMI, sprites TBL 0, background TBL 1
      (sta #x2000)

      (lda (! #b00011110)) ;; enable sprites, enable background, no left clipping
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

      ;; PPU Cleanup

      (lda (! #b10010000)) ;; enable NMI, sprites TBL 0, background TBL 1
      (sta #x2000)
      (lda (! #b00011110)) ;; enable sprites, background, no clipping on left.
      (sta #x2001)
      (lda (! 0)) ;; disable background scrolling
      (sta #x2005)
      (sta #x2005)

      (rti)

      (% ORG #xe000)

      (: "palette")
        ;;background palette
      (% BYTE #x22 #x29 #x1A #x0F   #x22 #x36 #x17 #x0F   #x22 #x30 #x21 #x0F   #x22 #x27 #x17 #x0F)
       ;;sprite palette
      (% BYTE #x22 #x1C #x15 #x14   #x22 #x02 #x38 #x3C   #x22 #x1C #x15 #x14   #x22 #x02 #x38 #x3C)

      (: "sprites")
      ;;      vert tile attr horiz
      (% BYTE #x80 #x32 #x00 #x80) ; 0 sprite
      (% BYTE #x80 #x33 #x00 #x88) ; 1
      (% BYTE #x88 #x34 #x00 #x80) ; 2
      (% BYTE #x88 #x35 #x00 #x88) ; 3

      (: "background")
      ;; sky
      (% BYTE #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24)
      (% BYTE #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24)

      ;; sky
      (% BYTE #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24)
      (% BYTE #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24)

      ;; some brick tops
      (% BYTE #x24 #x24 #x24 #x24 #x45 #x45 #x24 #x24 #x45 #x45 #x45 #x45 #x45 #x45 #x24 #x24)
      (% BYTE #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x53 #x54 #x24 #x24)

      ;;brick bottoms
      (% BYTE #x24 #x24 #x24 #x24 #x47 #x47 #x24 #x24 #x47 #x47 #x47 #x47 #x47 #x47 #x24 #x24)
      (% BYTE #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x55 #x56 #x24 #x24)

      (: "attribute")
      (% BYTE #b00000000  #b00010000  #b01010000  #b00010000  #b00000000  #b00000000  #b00000000  #b00110000)
      (% BYTE #x24 #x24 #x24 #x24  #x47 #x47 #x24 #x24  #x47 #x47 #x47 #x47  #x47 #x47 #x24 #x24)
      (% BYTE #x24 #x24 #x24 #x24  #x24 #x24 #x24 #x24  #x24 #x24 #x24 #x24  #x55 #x56 #x24 #x24)
      )

   (% SECTION "VECTORS" #xfffa 6
      (% WORD "nmi" "reset" 0))
   ))
