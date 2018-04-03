#lang racket-docs

;; Intended behavior
(define-data Hello
  [: - Nat]
  [Interpretation: "A natural number"])

;; Plain erronous behavior
#;(define-data Bad
  [: - Nat]
  [Interprtation: "A natural number"])

#;(define-data Badder
  [: - Nat])

#;(define typo Nat)

;; Poor behavior with case-sensitivity 
#;(define-data Hello2
  [: - typo
     - [Nat -> Bool]]
  [Interpretation: "Something special"])

#;(define-data Hello2
  [: - Typo
     - [Nat -> Bool]]
  [Interpretation: "Something special"])

;; yet this works
#;(define-data hello2
  [: - String]
  [Interpretation: "a lowercase"])

#;(define-data Foo
  [: - hello2]
  [Interpretation: "what happens here"])

;; Self-referential behavior
#;(define-data Other
    [: - Nat
       - [Listof Nat]]
    [Interpretation: "This is gonna work"])

#;(define-data Other
  [: - '()
     - (cons Hello Other)]
  [Interpretation: "This also works"])

;; Expressions are still a bit funky
#;(define-data Other2
  [: - Hello
     - (cons 0 Other2)]
  [Intepretation: "Should this work?"])

#;(define-data Other2
  [: - Any
     - [Listof Other2]]
  [Interpretation: "Doesn't work as expected"])

;; Documentation for things that don't exist
(define-docs helloworld
  [Signature: String]
  [Purpose: "Making something new"])

(define-docs (imagine a b c)
  [Signature: String -> Bool]
  [Purpose: "Causing a mess"])

