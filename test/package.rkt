#lang racket-docs

(define-docs hello
  [Signature: String]
  [Purpose: "Hello"]
  [Examples: hello => "Hello"])
(define hello "Hello")

(define-docs (+int x y)
  [Signature: Integer Integer -> Integer]
  [Purpose: "Adds 2 integers"])
(define +int +)