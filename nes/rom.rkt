#lang racket/base

(require "../bitutils.rkt"
         racket/port
         racket/draw
         racket/bytes
         racket/class
         racket/list)
         
(struct header (prg-pages
                chr-pages
                vertical-mirroring?
                battery-backed?
                trainer?
                four-screen?
                mapper
                8kram))

(struct rom (header prg chr))

(provide (struct-out rom)
         (struct-out header)
         print-rom-header
         parse-rom
         dump-chr)

(define (print-rom-header r)
  (for ([i `((,header-prg-pages . "16K PRG-ROM pages")
             (,header-chr-pages . "8K CHR-ROM pages")
	     (,header-mapper . "Cartridge mapper")
	     (,header-8kram . "8K RAM pages")
	     (,header-vertical-mirroring? . "Vertical mirroring")
	     (,header-battery-backed? . "Battery-backed ram")
	     (,header-trainer? . "Trainer")
	     (,header-four-screen? . "Four-screen mirroring"))])
    (printf "~a\t : ~a~n" ((car i) r) (cdr i))))

(define (parse-rom in)
  (define h (parse-header (read-bytes 16 in)))
  (rom h
       (read-bytes (* 16 1024 (header-prg-pages h)) in)
       (read-bytes (*  8 1024 (header-chr-pages h)) in)))

(define (parse-header h)
  (parameterize ([current-input-port (open-input-bytes h)])
    (unless (bytes=? #"NES\x1a" (read-bytes 4))
      (error "Bad ROM File"))
    (let ([prg (read-byte)]
	  [chr (read-byte)]
	  [ctl1 (read-byte)]
	  [ctl2 (read-byte)]
	  [8kram (read-byte)])
      (header prg
              chr
              (bitwise-bit-set? ctl1 0)
              (bitwise-bit-set? ctl1 1)
              (bitwise-bit-set? ctl1 2)
              (bitwise-bit-set? ctl1 3)
              (bitwise-ior (arithmetic-shift (bitwise-bit-field ctl2 4 8) 4)
                           (bitwise-bit-field ctl1 4 8))
              8kram))))

(define (dump-chr chr)
  (call-with-input-bytes chr
                         (λ (in)
                           (list (draw-patterns (get-tiles in))
                                 (draw-patterns (get-tiles in))))))

(define (get-tiles in)
  (for/list ([_ (in-range 256)])
    (let* ([tile (make-bitmap 8 8 #f)]
           [p1bs (read-bytes 8 in)]
           [p2bs (read-bytes 8 in)])
      (for ([i (in-range 8)]
            [p1row (in-bytes p1bs)]
            [p2row (in-bytes p2bs)])
        (let* ([colors (bytes-append*
                        (for/list ([j (in-range 7 -1 -1)])
                          (let ([b1 (bitwise-bit-set? p1row j)]
                                [b2 (bitwise-bit-set? p2row j)])
                            (cond [(and b2 b1) (bytes 255 255 255 255)]
                                  [b2 (bytes 255 192 192 192)]
                                  [b1 (bytes 255 64 64 64)]
                                  [else (bytes 0 0 0 0)]))))])
          (send tile set-argb-pixels 0 i 8 1 colors)))
      tile)))

(define (draw-patterns tiles)
  (define factor 2)
  (define columns 16)  
  (define surface (make-bitmap (* 8 factor columns)
                               (* 8 factor (/ (length tiles) columns))
                               #f))
  (define dc (new bitmap-dc% [bitmap surface]))  
  (send dc scale factor factor)
  (let draw-tiles ([tiles tiles] [row 0])
    (unless (empty? tiles)
      (define-values (tilerow next) (split-at tiles columns))
      (for ([t (in-list tilerow)]
            [col (in-range 0 (* 8 columns) 8)])
        (send dc draw-bitmap t col row))
      (draw-tiles next (+ row 8))))
  surface)
