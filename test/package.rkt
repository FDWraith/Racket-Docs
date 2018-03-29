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

(define-docs (+list x y)
  [Signature: [Listof String] [Listof String] -> [Listof String]]
  [Purpose: "Combines 2 lists"])
(define +list append)

(define-docs (+list-5 x y)
  [Signature: [Listof String] [Listof 5] -> [Listof String]]
  [Purpose: "Combines 2 lists"])
(define +list-5 append)

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

(+int 5 7)
(begin-without-type-checking
  (+int 3.5 3.6)
  (+int (+ 1 2.5) 3)
  (+list (list "Hello" 5 "world") (list "!"))
  (+list-5 (list "Hello" "world" "") (list 7)))
(+int 5 3)
(+list (list "Hello" "world" "") (list "!"))
(+list-5 (list "Hello" "world" "") (list 5))
(displayln "World")