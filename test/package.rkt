#lang racket-docs

(define-data String/False
  [: - String
     - #false]
  [Interpretation: "A string or false."]
  [Examples: "Hello" "World" #f])

(define-docs hello
  [Signature: String/False]
  [Purpose: "Hello"]
  [Examples: hello => "Hello"])
(define hello "Hello")

(define-docs (+int x y)
  [Signature: Int Int -> Int]
  [Purpose: "Adds 2 integers."])
(define +int +)

(define-docs provide
  [Syntax: (provide identifier ...)]
  [Semantics: "Exports @identifier, so other modules can import it."])

(begin-for-syntax
  #;(define-docs phase1-val
    [Signature: Int]
    [Purpose: "A value which could be used by macros"]
    [Examples: phase1-val => 5])
  (define phase1-val 5)
  
  (displayln "Hello"))

(define-docs (+ x y)
  [Signature: [Intersection [Int Int -> Int]
                            [Num Num -> Num]]]
  [Purpose: "Adds 2 numbers."]
  [Examples:
   (+ 2.5 3.5) => 6.0
   (+ 3 4) => 7])

(+int 5 7)
(begin-without-type-checking
  (+int 3.5 3.6)
  (+int (+ 1 2.5) 3))
(+int (+ 1 5) 3)
(displayln "World")