#lang racket/base

(require racket/gui/base
         racket/class)

(define frame (new frame%
                   [label "Example"]
                   [width 640]
                   [height 520]))

(define nescanvas%
  (class canvas%
    (define/override (on-char event)
      (when (eq? (send event get-key-code) #\q)
        (send frame show #f)))
    (super-new)))

(new nescanvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (send dc set-pen "red" 3 'solid)
        (send dc draw-point 0 0))])

(send frame show #t)
