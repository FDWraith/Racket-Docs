#lang racket-docs2

(define-docs hello
  [Signature: String/False]
  [Purpose: "Hello"]
  [Examples: hello => "Hello"])
(define hello "Hello")

(define-docs (+int x y)
  [Signature: Integer Integer -> Integer]
  [Purpose: "Adds 2 integers"])
(define +int +)

(define-data String/False
  [: - String
     - #false]
  [Interpretation: "A string or false."]
  [Examples: "Hello" "World" #f])

(define-docs provide
  [Syntax: (provide identifier ...)]
  [Semantics: "Exports @identifier, so other modules can import it."])