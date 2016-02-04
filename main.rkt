#lang racket/base

(require "assembler.rkt"
         "object.rkt"
         "nesrom.rkt"
         (prefix-in tut: "examples/tutorial.rkt")
         racket/file)

(define (inspect-rom romfile)
  (define rom (call-with-input-file romfile read-rom))
  (values (print-rom-header (rom-header rom))
          (dump-chr (rom-chr rom ))))

(define (make-rom obj out)
  (let ([hdr (find-section obj "HEADER")]
        [prg (find-section obj "PROGRAM")]
        [vec (find-section obj "VECTORS")]
        [chr (find-section obj "CHARS")])
    (for ([s (list hdr prg vec chr)])
      (write-bytes (section-seg s) out))))

(define (go)
  (call-with-output-file "background.nes" #:exists 'replace
    (lambda (out)
      (make-rom (assemble tut:background-test) out))))

(go)
(inspect-rom "background.nes")
