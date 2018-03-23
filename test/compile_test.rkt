#lang racket

(require "main.rkt")

(define-data Hello
  [: - Nat
     - String]
  [Interpretation: "What in the world"])

(define-data What
  [: Int]
  [Interpretation: "Huh"])

(compile-docs get-all-docs "hello.scrbl")