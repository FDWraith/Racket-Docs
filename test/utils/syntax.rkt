#lang racket

(require [for-syntax "../../lib/racket-docs/utils/syntax.rkt"]
         "../../lib/racket-docs/utils/syntax.rkt"
         syntax/parse
         rackunit)

(define-syntax one (mk-id-macro #'1))

(struct foo-box [x] #:transparent)

(check-equal? (+ one 3.5) 4.5)

(check-equal? (syntax->datum (flatten/stx #'((a b c) (d) (e f))))
              (syntax->datum #'(a b c d e f)))

(check-equal?
 (syntax->datum
  (syntax-parse #'(#:description "hello" #:datum-literals (foo bar baz) 5)
    [(x:stxclass-option ... y) #'(x ... ((y)))]))
 (syntax->datum
  #'((#:description "hello") (#:datum-literals (foo bar baz)) ((5)))))
(check-not-exn
 (λ () (syntax-parse #'(foo foo)
         [(x (~var _ (temp-matches #'x))) #'ok])))
(check-not-exn
 (λ () (syntax-parse #'(foo _)
         [(x (~var _ (temp-matches #'x))) #'ok])))
(check-exn
 exn:fail?
 (λ () (syntax-parse #'(foo bar)
         [(x (~var _ (temp-matches #'x))) #'ok])))
(check-true (equal-datum? #'foo #'foo))
(check-false (equal-datum? #'foo #'bar))
(check-true (equal-datum? 'foo 'foo))
(check-false (equal-datum? 'foo 'bar))
(check-true (equal-datum? (foo-box #'foo) (foo-box #'foo)))
(check-false (equal-datum? (foo-box #'foo) (foo-box #'bar)))