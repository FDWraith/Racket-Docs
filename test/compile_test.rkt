#lang racket-docs

(define-data Hello
  [: - Nat
     - String]
  [Interpretation: "What in the world"]
  [Examples:
   1
   "Hello"])

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
