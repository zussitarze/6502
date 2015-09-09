#lang racket/base

(require rackunit
         "core.rkt"
         "assembler.rkt"
         "bitutils.rkt"
         "object.rkt")

(define-syntax 6502-test-case
  (syntax-rules ()
    [(6502-test-case name initials source expectations)
     (test-case name
       (define obj (assemble source))
       (define mem (make-bytes (* 64 1024) 0))
       (for ([i initials])
         (bytes-set! mem (car i) (cdr i)))
       (execute (loader obj #:use-ext-memory mem) (section-start (car obj)))
       (with-check-info (['obj (dumpobject obj)]
                         ['page0 (dumpbytes (subbytes mem #x000 #x100))]
                         ['page1 (dumpbytes (subbytes mem #x100 #x200))]
                         ['page2 (dumpbytes (subbytes mem #x200 #x300))])
         (for ([e expectations])
           (check-eq? (bytes-ref mem (car e)) (cdr e)))))]))

(6502-test-case
 "All kinds of storage"
 '()
 (6502asm
  (lda (! 1))
  (sta #x50) ; #x50 <= 1
  ;;-------------------------
  (tax)
  (inx)
  (stx #x51) ; #x51 <= 2
  ;;-------------------------
  (txa)
  (tay)
  (iny)  
  (sty #x52) ; #x52 <= 3
  ;;-------------------------
  (lda (! 4))
  (ldx (! 3))
  (sta #x50 X) ; #x53 <= 4
  ;;-------------------------
  (lda (! 5))
  (sta #x0250 X) ; #x0253 <= 5
  ;;-------------------------
  (lda (! #x55))
  (sta #x54)
  (lda (! #x02))
  (sta #x55)
  (lda (! 6))
  (ldx (! 4))
  (sta (@ #x50 X)) ; #x0255 <= 6
  ;;-------------------------
  (ldy (! 7))
  (lda (! 7))
  (sta (@ #x54) Y) ; #x025C <= 7
  ;;-------------------------
  (ldx (! 8))
  (ldy (! 2))
  (stx #x56 Y) ; #x58 <= 8  
  ;;-------------------------
  (lda (! 9))
  (ldy (! 2))
  (sta #x0256 Y) ;; #x0258 <= 9
  )
 '((#x50 . 1) (#x51 . 2) (#x52 . 3) (#x53 . 4)
   (#x0253 . 5) (#x0255 . 6) (#x025C . 7) (#x58 . 8) (#x0258 . 9)))

(6502-test-case
 "Find max (post-indexed)"
 '((#x41 . 5) (#x42 . 00) (#x43 . #x02)
   (#x201 . #x67) (#x202 . #x79) (#x203 . 15) (#x204 . #xe3) (#x205 . #x72))
 (6502asm
  (ldy #x41)
  (lda (! 0))
  (: "maximum")
  (cmp (@ #x42) Y)
  (bcs "no change")
  (lda (@ #x42) Y)
  (: "no change")
  (dey)
  (bne "maximum")
  (sta #x40)
  (brk))
 '((#x40 . #xe3)))

(6502-test-case
 "Pre-indexed test"
 ;; get the value at address (#x60 + 0) and store it at the address (#x60 + 2)
 '((#x60 . #x2f) (#x61 . #x02) (#x62 . #x32) (#x63 . #x02)
   (#x022f . #x88))
 (6502asm
  (% EQU "nil" 0)
  (ldx (! "nil"))
  (lda (@ #x60 X))
  (inx)
  (inx)
  (sta (@ #x60 X))
  (brk))
 '((#x0232 . #x88)))

(6502-test-case
 "wordsplit2"
 '((#x40 . #x3f))
 (6502asm
  (lda #x40)
  (and (! #b00001111))
  (sta #x42)
  (lda #x40)
  (lsr A)
  (lsr A)
  (lsr A)
  (lsr A)
  (sta #x41))
 '((#x41 . #x03) (#x42 . #x0f)))

(6502-test-case
 "16bit add"
 '((#x40 . #x2A) (#x41 . #x67) (#x42 . #xf8) (#x43 . #x14))
 (6502asm
  (clc)
  (lda #x40)
  (adc #x42)
  (sta #x44)
  (lda #x41)
  (adc #x43)
  (sta #x45))
 '((#x44 . #x22) (#x45 . #x7c)))

(6502-test-case
 "Tests jumping and branching across segments"
 '()
 (6502asm
   (% ORG 10)
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
       (brk))))
 '((#x99 . #x33)))

(6502-test-case
 "Lookup square"
 '()
 (6502asm
  (ldx (! 4))
  (lda #x60 X)
  (sta #x90)
  (ldx (! 7))
  (lda #x60 X)
  (sta #x91)
  (brk)
  (% ORG #x60)
  (: "SQTAB")
  (% BYTE 0 1 4 9 16 25 36 49))
 '((#x90 . 16) (#x91 . 49)))

(6502-test-case
 "Adding sums of data"
 '((#x41 . #x03) (#x42 . #x28) (#x43 . #x55) (#x44 . #x26))
 (6502asm
   (lda (! 0))
   (ldx #x41)
   (: "SUMD")
   (clc)
   (adc #x41 X)
   (dex)
   (bne "SUMD")
   (sta #x40)
   (brk))
 '((#x40 . #xa3)))

(6502-test-case
 "Count negatives"
 '((#x41 . #x06) (#x42 . #x68) (#x43 . #xf2) (#x44 . #x87)
   (#x45 . #x30) (#x46 . #x59) (#x47 . #x2a))
 (6502asm
  (ldx (! 0))
  (ldy (! 0))
  (: "Sum")
  (lda #x42 X)
  (bpl "Next")
  (iny)
  (: "Next")
  (inx)
  (cpx #x41)
  (bne "Sum")
  (sty #x40)
  (brk))
 '((#x40 . #x02)))

(6502-test-case
 "String length"
 '()
 (6502asm
   (ldx (! 0))
   (lda (! (char->integer #\newline)))
   (: "Check")
   (cmp "STR" X)
   (beq "Done")
   (inx)
   (jmp "Check")
   (: "Done")
   (stx #x40)
   (brk)
   (: "STR")
   (% STRING "Hello World!\n"))
 '((#x40 . 12)))   

(6502-test-case
 "Bubble sort"
 '((#x40 . 6)
   (#x41 . #x2a) (#x42 . #xb5) (#x43 . #x60) (#x44 . #x3f) (#x45 . #xd1) (#x46 . #x19))
 (6502asm
   (: "sort")
   (ldy (! 0))
   (ldx #x40)
   (dex)
   (: "pass")
   (lda #x40 X)
   (cmp #x41 X)
   (bcc "count")
   (ldy (! 1))
   (pha)
   (lda #x41 X)
   (sta #x40 X)
   (pla)
   (sta #x41 X)
   (: "count")
   (dex)
   (bne "pass")
   (dey)
   (beq "sort")      
   (brk))
 '((#x41 . #x19) (#x42 . #x2a) (#x43 . #x3f) (#x44 . #x60) (#x45 . #xb5) (#x46 . #xd1)))

(6502-test-case
 "Simple subroutine"
 '((#x40 . 13))
 (6502asm
  (ldx (! #xff))
  (txs)
  (lda #x40)
  (jsr "ASDEC")
  (sta #x41)
  (brk)
  ;; ASDEC: Convert values to ascii digits.
  (% ORG #x20)
  (: "ASDEC")
  (cmp (! 10)) ;; is digit bigger than 10?
  (bcc "ASCZ")
  (clc)
  (adc (! 7))
  (: "ASCZ")
  (adc (! (char->integer #\0)))
  (rts))
 `((#x41 . ,(char->integer #\D))))
