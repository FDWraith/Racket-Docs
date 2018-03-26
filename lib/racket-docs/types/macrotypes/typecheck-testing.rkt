#lang racket
(require (for-syntax syntax/parse
                     "typecheck.rkt"
                     rackunit)
         rackunit)

; wish list
(provide
 ; (check-type Term : Type)
 ; (check-type Term : Type => Term)
 ; Fails if Term does not have type Type.
 ; If given a result Term, fails if given expr does not eval to that result.
 check-type

 ; A Msg is a String
 
 ; (typecheck-fail Term Msg)
 ; Test passes if typechecking the term fails with msg containing given Msg.
 typecheck-fail)

(define-syntax check-type
  (syntax-parser
    [(_ e (~datum :) expected:type)
     #:with (_ actual) (type-of #'e)
     #:fail-unless (type=? #'actual #'expected) (tyerr-msg #'expected #'actual)
     #'(void)]
    ;; this second clause tests both type checking and the run time result
    [(_ e (~datum :) expected:type (~datum =>) expected-val)
     #:with (e+ actual) (type-of #'e)
     #:fail-unless (type=? #'actual #'expected) (tyerr-msg #'expected #'actual)
     #'(check-equal? e+ expected-val)]))

(define-syntax typecheck-fail
  (syntax-parser
    [(_ e #:with-msg m:string)
     (check-exn (pregexp (syntax->datum #'m))
                (λ () (type-of #'e)))
     #'(void)]))
