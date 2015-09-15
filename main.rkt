#lang racket/base

(require 6502/assembler
         racket/file
         "rom.rkt"
         (prefix-in tut: "examples/tutorial.rkt"))

(define (inspect-rom romfile)
  (define r (call-with-input-file romfile parse-rom))
  (values (print-rom-header (rom-header r))
          (dump-chr (rom-chr r))))

(define (make-rom obj out)
  (define (sec n)
    (or (findf (λ (s) (string=? n (section-name s))) obj)
        (error "Missing section:" n)))    
  (let ([hdr (sec "HEADER")]
        [prg (sec "PROGRAM")]
        [vec (sec "VECTORS")]
        [chr (sec "CHARS")])
    (for ([s (list hdr prg vec chr)])
      (write-bytes (section-seg s) out))))

(define (go)
  (call-with-output-file "controller.nes" #:exists 'replace
    (lambda (out)
      (make-rom (assemble tut:controller-test) out))))

(go)
(inspect-rom "sprite.nes")
