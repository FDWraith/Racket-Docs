#lang racket

(require [for-syntax "../../lib/utils/parse-class.rkt"
                     syntax/parse]
         rackunit)

(begin-for-syntax
  (define-parse-class foo
    #:datum-literals (3*)
    [n:nat (* (syntax-e #'n) 2)]
    [(3* n:nat) (* (syntax-e #'n) 3)])

  (define-parse-class bar
    #:datum-literals (3+)
    [n:nat (+ (syntax-e #'n) 2)]
    [(3+ n:nat) (+ (syntax-e #'n) 3)])

  (define-splicing-parse-class baz
    #:datum-literals (3+)
    [(~seq (~literal x) s:string) (syntax-e #'s)]))

(define-syntax (vals stx)
  (syntax-parse stx
    [(_ foo:foo ...)
     (datum->syntax stx (foldr + 0 (parse-classes (foo ...))))]
    [(_ bar:bar ...)
     (datum->syntax stx (foldr * 1 (parse-classes (bar ...))))]
    [(_ baz:baz ...)
     (datum->syntax stx (foldr string-append "" (parse-classes (baz ...))))]))

(check-equal? (vals 3 4 (3* 5) 2) 33) ; Matches foos
(check-equal? (vals 3 4 (3+ 5) 2) 960) ; Matches bars
(check-equal? (vals x "hello" x " " x "world") "hello world") ; Matches bazs
(check-equal? (vals 7) 14) ; Matches foos (precedence)