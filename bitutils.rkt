#lang racket/base

(require (for-syntax racket/base)
         racket/string
         racket/format
         racket/list
         racket/require)

(require racket/fixnum)
;; (require (filtered-in
;;           (λ (name) (regexp-replace #rx"unsafe-" name ""))
;;           racket/unsafe/ops))


(provide fx+multi 8bit+ 16bit+
         dumpbytes dumpobject)

(define-syntax fx+multi
  (syntax-rules ()
    [(fx+multi x y) (fx+ x y)]
    [(fx+multi x y rest ...) (fx+multi (fx+ x y) rest ...)]))

(define-syntax 8bit+
  (syntax-rules ()
    [(8bit+ b ...)
     (fxand #xff (fx+multi b ...))]))

(define-syntax 16bit+
  (syntax-rules ()
    [(16bit+ b ...)
     (fxand #xffff (fx+multi b ...))]))

(define (dumpbytes bs [trim-leading-zeros #t])
  (define (hex bs)
    (string-join (map (lambda (b)
                        (~r b #:base 16 #:min-width 2 #:pad-string "0"))
                      bs)
                 " "))
  (if trim-leading-zeros
      (let* ([l (bytes->list bs)]
             [dpl (dropf l zero?)]
             [zs (- (length l) (length dpl) 1)])
        (string-append (if (> zs 0) (format "<0 ... ~a> " zs) "")
                       (hex dpl)))
      (hex (bytes->list bs))))

(define (dumpobject obj)
  (map (lambda (s)
         (cons (car s) (dumpbytes (cdr s) #f)))
       obj))
