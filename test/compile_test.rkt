#lang racket

(require "../lib/racket-docs/main.rkt")

(define-data Hello
  [: - Nat
     - String]
  [Interpretation: "What in the world"]
  [Examples: 1])

(define-docs (hello o)
  [Signature: Hello -> Bool]
  [Purpose: "Returns a lot of annoying things"])
(define (hello o)
  #f)

(define-data What
  [: Int -> Bool]
  [Interpretation: "Huh"]
  [Examples:
   (λ (i) #false)])

(define-docs WORLD_STATE
  [Signature: Nat]
  [Purpose: "Describes the state of the world"])
(define WORLD_STATE 20)

(begin-for-syntax
  (compile-docs (get-all-docs)))