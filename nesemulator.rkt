#lang racket/base

(require "nesrom.rkt"
         "bitutils.rkt"
         (prefix-in core: "core.rkt")
         racket/match)

(require racket/fixnum)
;; (require (filtered-in
;;           (λ (name) (regexp-replace #rx"unsafe-" name ""))
;;           racket/unsafe/ops))

(define (connect-bus rom)
  (define ram (make-bytes #x2000 0))
  (define prg (case (header-prg-pages (rom-header rom))
                [(1) (bytes-append (rom-prg rom) (rom-prg rom))]
                [(2) (rom-prg rom)]))
  (case-lambda
    [() ram]
    [(addr)
     (cond
       [(< addr #x2000) (bytes-ref ram addr)]
       [(< addr #x4020) 0]
       [(< addr #x8000) (error "Cannot handle sram")]
       [(< addr #x10000) (bytes-ref prg (fx- addr #x8000))])]
    [(addr val)
     (cond
       [(< addr #x2000) (bytes-set! ram addr val)]
       [(< addr #x4020) (void)]
       [(< addr #x8000) (error "Cannot handle sram")]
       [(< addr #x10000) (error "Cannot write to PRG ROM")])]))

(define (go)
  (call-with-input-file "background.nes"
    (λ (in)
      (define rom (read-rom in))
      (print-rom-header (rom-header rom))
      (define bus (connect-bus rom))
      (core:execute bus))))
      ;; (define-values (opcount register status memory) )
      ;; (values (dumpbytes (subbytes memory #x0000 #x0100))
      ;;         (dumpbytes (subbytes memory #x0100 #x0200))
      ;;         (dumpbytes (subbytes memory #x0200 #x0300))
      ;;         register status))))

(define t (go))

(define (stop)
  (kill-thread t))

(thread-dead? t)
