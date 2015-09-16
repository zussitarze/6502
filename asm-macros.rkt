#lang racket/base

(require racket/list
         racket/splicing
         (for-syntax racket/base))

(provide 6502asm)

(define lblcount (make-parameter 0))

(define-syntax (6502asm stx)
  (syntax-case stx ()
    [(asm l ...) 
     #`(parameterize ([lblcount 0])
         (flatten-lvl1
          (list #,@(map parse-asm-line (syntax->list #'(l ...))))))]))

(define-for-syntax (parse-asm-line stx)
  (define (normalize-sym s)
    (string->symbol (string-downcase (symbol->string (syntax->datum s)))))
  (syntax-case stx (: % EQU SECTION ORG BYTE WORD STRING INCLUDE FILE LOCAL FOR IF)
    [(% EQU n v) #'(list 'equ n v)] 
    [(% SECTION n s l (op ...))
     #`(list 'section n s l
             (flatten-lvl1 (list #,@(map parse-asm-line (syntax->list #'(op ...))))))]
    [(% ORG l) #'(list 'origin l)]
    [(% BYTE bs ...) #'(list 'data-bytes bs ...)]
    [(% WORD ws ...) #'(list 'data-words ws ...)]
    [(% STRING s) #'(list 'data-string s)]
    [(% INCLUDE i) #'i]
    [(% FILE f) #'(list 'file f)]
    [(% LOCAL (name ...) body ...)
     #`(let ([name (local-label 'name)] ...)
         (lblcount (+ (lblcount) 1))
         (list #,@(map parse-asm-line (syntax->list #'(body ...)))))]
    [(% FOR (binding ...) body ...)
     #`(append*
        (for/list (binding ...)
          (list 
           #,@(map parse-asm-line (syntax->list #'(body ...))))))]
    [(% IF test t f)
     #`(if test #,(parse-asm-line #'t) #,(parse-asm-line #'f))]
    [(% other ...)
     (raise-syntax-error #f "Invalid pseudo instruction" stx)]
    [(: l) #'(list 'label l)]
    [(mnemonic arg ...)
     (with-syntax
       ([(amode operand)
         (syntax-case #'(arg ...) (! @ ^ X Y A)
                      [((! v)) #'('stx-immediate v)]
                      [((@ v X)) #'('stx-pre-indexed-x v)]
                      [((@ v) Y) #'('stx-post-indexed-y v)]
                      [((@ v)) #'('stx-indirect v)]
                      [(A) #'('stx-accumulator 'none)]
                      [(v X) #'('stx-idx-x v)]
                      [(v Y) #'('stx-idx-y v)]
                      [(v) (if (memq (normalize-sym #'mnemonic) '(bcc bcs beq bne bmi bpl bvc bvs))
                               #'('stx-relative v)
                               #'('stx-direct v))]
                      [() #'('stx-implied 0)])])
       #`(list 'operation '#,(normalize-sym #'mnemonic) amode operand))]))

(define (local-label n)
  (string-append (symbol->string n) (number->string (lblcount))))

(define (flatten-lvl1 lst)
  (define (f lst acc)
    (foldr (λ (x acc)
             (if (list? (car x))
                 (f x acc)
                 (cons x acc)))
           acc lst))
  (f lst '()))
