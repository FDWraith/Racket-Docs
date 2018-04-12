#lang racket

(provide tests-for-props
         tests-for-props1)

(require [for-syntax syntax/parse]
         "../types/error.rkt"
         "../types/subtype.rkt"
         "../struct.rkt"
         syntax/parse
         
         [for-template "../types/language.rkt"
                       "../types/use.rkt"
                       "../utils.rkt"
                       rackunit
                       racket/base])

#;(define-docs (tests-for-props doc-props)
    [Signature: [Listof DocProp] -> [Maybe Syntax]]
    [Purpose: #<<"
If one of the properties provides examples,
the result syntax will add the examples as tests.
Otherwise returns #false.
"
              ])
(define (tests-for-props doc-props)
  (define example-prop (findf (curry prop-has-type? 'examples) doc-props))
  (and example-prop
       (tests-for-examples (doc-prop-value example-prop))))

#;(define-docs (tests-for-props1 doc-props)
    [Signature: [Listof DocProp] -> [Maybe Syntax]]
    [Purpose: #<<"
If one of the properties provides examples,
the result syntax will add the examples as tests.
Otherwise returns #false.
This adds tests in phase 1, using a different module (test1)
so that they don't conflict with tests in phase 0.
"
              ])
(define (tests-for-props1 doc-props)
  (define example-prop (findf (curry prop-has-type? 'examples) doc-props))
  (and example-prop
       (tests-for-examples1 (doc-prop-value example-prop))))

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
    [Purpose: "Returns syntax which verifies the example."]
    [Examples:
     (test-for-example (eval-example #'(+ 1 2) #'3)) =>
     #'(check-equal? (+ 1 2) 3)])
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
       (test-for-data-example (plain-data-example-expr example)
                              (plain-data-example-type example))]
      [(plain-data-example? example)
       (test-for-data-example (interpret-data-example-expr example)
                              (interpret-data-example-type example))]))
  (define test-stx (example-stx example))
  (datum->syntax test-stx
                 test-datum
                 test-stx
                 test-stx))

#;(define-docs (test-for-data-example expr-stx type-stx)
    [Signature: Syntax [Stx Type] -> Syntax]
    [Purpose: #<<"
Returns syntax which verifies the data example with the given expression and
type.
"
              ])
(define (test-for-data-example expr-stx type-stx)
  #`(begin
      #,(type-test-for-data-example expr-stx type-stx)
      #,(eval-test-for-data-example expr-stx)))

#;(define-docs (type-test-for-data-example expr-stx type-stx)
    [Signature: Syntax [Stx Type] -> Syntax]
    [Purpose: #<<"
Returns syntax which verifies the given expression conforms to the given type.
"
              ])
(define (type-test-for-data-example expr-stx type-stx)
  #`(assert-type/phaseless [#,expr-stx : #,type-stx]
                           "Example not an instance of the data definition"))

#;(define-docs (eval-test-for-data-example expr-stx type-stx)
    [Signature: Syntax -> Syntax]
    [Purpose: #<<"
Returns syntax which verifies the given expression doesn't throw a runtime
error.
"
              ])
(define (eval-test-for-data-example expr-stx)
  (list #'check-not-exn
        #`(λ () #,expr-stx)
        "While evaluating an example"))