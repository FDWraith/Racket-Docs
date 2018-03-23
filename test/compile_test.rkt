#lang racket

(require "../lib/racket-docs/main.rkt")

(define-data Hello
  [: - Nat
     - String]
  [Interpretation: "What in the world"])

(define-docs hello
  [Signature: Hello -> Bool]
  [Purpose: "Returns a lot of annoying things"])
(define (hello o)
  #f)

(define-data What
  [: Int]
  [Interpretation: "Huh"])

