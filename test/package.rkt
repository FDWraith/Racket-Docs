#lang racket-docs

(begin-for-syntax
  (define-data StxDocumentedInt
    [: Int]
    [Interpretation: "A data definition created in phase 1."]
    [Examples: 5])

  (define-data [Either X Y]
    [: - X
       - Y]
    [Interpretation: "A data definition constructor."]))

#;(define-data [Either X Y]
  [: - X
     - Y]
  [Interpretation: "A data definition constructor."])

(define-data String/False
  [: - String
     - Bool]
  [Interpretation: "A string or false."]
  [Examples: "Hello" "World" #f])

(define-docs hello
  [Signature: String/False]
  [Purpose: "Hello"]
  [Examples: hello => "Hello"])
(define hello "Hello")

(define-docs (+int x y)
  [Signature: StxDocumentedInt Int -> Int]
  [Purpose: "Adds 2 integers."])
(define +int +)

(define-docs (+list x y)
  [Signature: [Listof String] [Listof String] -> [Listof String]]
  [Purpose: "Combines 2 lists"])
(define +list append)

(define-docs (+tuple x y)
  [Signature: (list String Int Nat)
              (list String Int Nat) ->
              (list String Int Nat)]
  [Purpose: "Combines 2 tuples"])
(define (+tuple x y)
  (list (string-append (second x) (first y))
          (+ (first x) (second y))
          (+ (third x) (third y)))
  (list (string-append (first x) (first y))
        (+ (second x) (second y))
        (+ (third x) (third y))))

(define-docs (+list-5 x y)
  [Signature: [Listof String] [Listof 5] -> [Listof String]]
  [Purpose: "Combines 2 lists"])
(define +list-5 append)

#;(define-docs provide
  [Syntax: (provide identifier ...)]
  [Semantics: "Exports @identifier, so other modules can import it."])

#;(begin-for-syntax
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
  (+tuple (list "a" 1 -2) (list "b" -1 3))
  (+list-5 (list "Hello" "world" "") (list 7)))
(+int 5 3)
(+list (list "Hello" "world" "") (list "!"))
(+tuple (list "a" 1 2) (list "b" -1 3))
(+list-5 (list "Hello" "world" "") (list 5))
(displayln "World")