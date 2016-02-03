#lang racket/base

(require "bitutils.rkt")

(provide (struct-out section)
         dumpobject
         load-object)

(struct section (name start seg)
  #:transparent)

(define (load-object obj mem)
  (for/fold ([prev (section-start (car obj))])
            ([sec (in-list obj)])
    (when (< (section-start sec) prev)
      (error "Loader error: Object file contains overlapping segment at" (section-start sec)))
    (bytes-copy! mem (section-start sec) (section-seg sec))
    (+ prev (bytes-length (section-seg sec))))
  )

(define (dumpobject obj)
  (map (lambda (s)
         (list (section-start s)
               (dumpbytes (section-seg s) #f)
               (section-name s)))
       obj))
