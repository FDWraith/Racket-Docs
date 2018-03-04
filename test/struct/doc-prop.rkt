#lang racket

(require "../../lib/struct/doc-prop.rkt"
         rackunit)

(define ex1 (type-doc-prop '[Int -> String]))
(define ex2 (type-doc-prop '[Int -> Int]))
(define ex3 (purpose-doc-prop "Converts the integer into a string"))
(define ex4 (purpose-doc-prop "Returns the integer"))

(check-equal? (check-shared-types (list ex1 ex2 ex3 ex4))
              'type)
(check-equal? (check-shared-types (list ex1 ex2 ex3))
              'type)
(check-equal? (check-shared-types (list ex3 ex4))
              'purpose)
(check-equal? (check-shared-types (list ex1))
              #false)
(check-equal? (check-shared-types (list))
              #false)