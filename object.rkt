#lang racket/base

(require "bitutils.rkt")

(provide (struct-out section)
         dumpobject)

(struct section (name start seg)
  #:transparent)

(define (dumpobject obj)
  (map (lambda (s)
         (list (section-start s)
               (dumpbytes (section-seg s) #f)
               (section-name s)))
       obj))
