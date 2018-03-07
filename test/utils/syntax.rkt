#lang racket

(require [for-syntax "../../lib/racket-docs/utils/syntax.rkt"
                     syntax/parse]
         "../../lib/racket-docs/utils/syntax.rkt"
         rackunit)

(begin-for-syntax
  (define-syntax-class test-class
    [pattern ((~datum 1+) x)
             #:attr attr (add1 (syntax-e #'x))]
    [pattern ((~datum 1-) x)
             #:attr attr (sub1 (syntax-e #'x))]))

(define-syntax (test stx)
  (syntax-parse stx
    [(_ x:test-class ...)
     (datum->syntax stx (foldl + 0 (attributes (x.attr ...))))]))

(define-syntax one (mk-id-macro #'1))

(struct foo-box [x] #:transparent)

(check-equal? (test (1+ 1) (1- 2) (1+ 3)) 7)
(check-equal? (test (1- 1) (1+ 2) (1- 3)) 5)

(check-equal? (+ one 3.5) 4.5)

(check-equal? (syntax->datum (flatten/stx #'((a b c) (d) (e f))))
              (syntax->datum #'(a b c d e f)))

(check-true (equal-datum? #'foo #'foo))
(check-false (equal-datum? #'foo #'bar))
(check-true (equal-datum? 'foo 'foo))
(check-false (equal-datum? 'foo 'bar))
(check-true (equal-datum? (foo-box #'foo) (foo-box #'foo)))
(check-false (equal-datum? (foo-box #'foo) (foo-box #'bar)))