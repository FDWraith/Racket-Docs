#lang racket

(provide tests-for-props)

(require "../struct.rkt"
         syntax/parse
         [for-template rackunit
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
  (cond
    [(eval-example? example)
     (define res-datum
       (list #'check-equal?
             (eval-example-expr example)
             (eval-example-expected example)))
     (define res-stx (eval-example-stx example))
     (datum->syntax res-stx
                    res-datum
                    res-stx
                    res-stx)]
    [(plain-data-example? example)
     (plain-data-example-expr example)]
    [(interpret-data-example? example)
     (interpret-data-example-expr example)]))