#lang racket

(provide tests-for-props
         tests-for-props1)

(require [for-syntax syntax/parse]
         "../struct.rkt"
         syntax/parse
         
         [for-template "../utils.rkt"
                       rackunit
                       racket/base])
    
#;(define-docs (tests-for-props doc-props)
    [Signature: [Listof DocProp] -> Syntax]
    [Purpose: #<<"
If one of the properties provides examples,
the result syntax will add the examples as tests.
Otherwise, the result syntax will do nothing.
"
              ])
(define (tests-for-props doc-props)
  (define example-prop (findf (curry prop-has-type? 'examples) doc-props))
  (if example-prop
      (tests-for-examples (doc-prop-value example-prop))
      #'(void)))

#;(define-docs (tests-for-props1 doc-props)
    [Signature: [Listof DocProp] -> Syntax]
    [Purpose: #<<"
If one of the properties provides examples,
the result syntax will add the examples as tests.
Otherwise, the result syntax will do nothing.
This adds tests in phase 1, using a different module (test1)
so that they don't conflict with tests in phase 0.
"
              ])
(define (tests-for-props1 doc-props)
  (define example-prop (findf (curry prop-has-type? 'examples) doc-props))
  (if example-prop
      (tests-for-examples1 (doc-prop-value example-prop))
      #'(void)))

#;(define-docs (tests-for-examples examples)
    [Signature: [Listof Example] -> Syntax]
    [Purpose: #<<"
The result syntax adds tests from the examples to the test submodule.
"
              ]
    [Examples:
     (test-for-examples
      (list (eval-example #'(+ 1 2) #'3)
            (plain-data-example #'(println "Foo")))) =>
     #'(module+ test
         (check-equal? (+ 1 2) 3)
         (println "Foo"))])
(define (tests-for-examples examples)
  #`(module+ test
      #,@(map test-for-example examples)))

#;(define-docs (tests-for-examples1 examples)
    [Signature: [Listof Example] -> Syntax]
    [Purpose: #<<"
The result syntax adds tests from the examples to the test submodule.
This adds tests in phase 1, using a different module (test1)
so that they don't conflict with tests in phase 0.
"
              ]
    [Examples:
     (test-for-examples1
      (list (eval-example #'(+ 1 2) #'3)
            (plain-data-example #'(println "Foo")))) =>
     #'(module+ test1
         (check-equal? (+ 1 2) 3)
         (println "Foo"))])
(define (tests-for-examples1 examples)
  #`(module+ phase1-test
      #,@(map test-for-example examples)))

#;(define-docs (test-for-example example)
    [Signature: Example -> Syntax]
    [Purpose: "Returns a syntax which verifies the example."]
    [Examples:
     (test-for-example (eval-example #'(+ 1 2) #'3)) =>
     #'(check-equal? (+ 1 2) 3)
     (test-for-example (plain-data-example #'(println "Foo"))) =>
     #'(println "Foo")
     (test-for-example (interpret-data-example #'(println "a") "Prints a.")) =>
     #'(println "a")])
(define (test-for-example example)
  (define test-datum
    (cond
      [(eval-example? example)
       (list #'check
             #'equal-datum?
             (eval-example-expr example)
             (eval-example-expected example)
             #'"Example doesn't evaluate to what's expected")]
      [(plain-data-example? example)
       (list #'check-not-exn
             #`(λ () #,(plain-data-example-expr example))
             "While evaluating an example")]
      [(interpret-data-example? example)
       (list #'check-not-exn
             #`(λ () #,(interpret-data-example-expr example))
             "While evaluating an interpreted example")]))
  (define test-stx (example-stx example))
  (datum->syntax test-stx
                 test-datum
                 test-stx
                 test-stx))