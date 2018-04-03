#lang racket-docs

; ------------------------------------------------------------------------------
; Racket (Fundamentals 1)

; An OpChain is one of:
; - (cons Number '())
; - (cons Number (cons Operator OpChain))
; INTERPRETATION A combination of infix operators and
; numbers, without parenthesis.
; EXAMPLES
(define ex1 (cons 4 '())) ; 4
(define ex2 (cons 1 (cons '+ (cons 2 (cons '* (cons 3 '()))))))  ; 1 + 2 * 3
(define ex3 (cons -3.5 (cons '/ (cons 1/2 '())))) ; -3.5 / 1/2

; An Operator is one of:
; - '+
; - '-
; - '*
; - '/
; INTERPRETATION A mathematical operator, describes how
; 2 numbers are combined.

; ------------------------------------------------------------------------------
; Racket+Docs

(define-data OpChain
  [: - (cons Num '())
     - (cons Num (cons Operator OpChain))]
  [Interpretation:
   "A combination of infix operators and numbers, without parenthesis."]
  [Examples:
   (cons 4 '()) <= "4"
   (cons 1 (cons '+ (cons 2 (cons '* (cons 3 '()))))) <= "1 + 2 * 3"
   (cons -3.5 (cons '/ (cons 1/2 '()))) <= "-3.5 / 1/2"])

(define-data Operator
  [: - '+
     - '-
     - '*
     - '/]
  [Interpretation:
   "A mathematical operator, describes how 2 numbers are combined."])

; ------------------------------------------------------------------------------

(define-data Natural
  [: - 0
     - (add1 Natural)]
  [Interpretation: "A natural number."]
  [Examples:
   0
   (add1 (add1 (add1 0))) <= "3"])

(define-docs (string* str n)
  [Signature: String Nat -> String]
  [Purpose: "Appends the given @str to itself @n times."]
  [Examples:
   (string* "ABC" 0) => ""
   (string* "ABC" 1) => "ABC"
   (string* "Foo!" 3) => "Foo!Foo!Foo!"
   (string* "Hello" 4) => "HelloHelloHelloHello"])
(define (string* str n)
  (cond
    [(zero? n) ""]
    [(positive? n) (string-append str (string* str (- n 1)))]))

(define-data [Tree X]
  [: (tree X [Listof [Tree X]])]
  [Interpretation: #<<"
A tree.
@value refers to the top node.
@children refers to child trees. Each @value in @children is a child node,
each @value in @children of @children is a grandchild node, and so on.
"
            ]
  [Examples:
   (tree 12 '())
   (tree "A" (list (tree "B" '()) (tree "C" (list (tree "D" '())))))
   (tree (tree "Complex" '()) (list (tree "Tree" '())))])
(struct tree (value children) #:transparent)

(define-docs (mk-tree value children)
  [Signature: {All X} X [Listof [Tree X]] -> [Tree X]]
  [Purpose: "Creates a tree."])
(define (mk-tree value children)
  (tree value children))

(define-docs (annotate-depth t0)
  [Signature: {All X} [Tree X] -> [Tree (list X Nat)]]
  [Purpose: #<<"
Pairs each element in the @tree with its level, where the root of the tree
has level 0.
"
            ]
  [Examples:
   (annotate-depth (tree 'x '())) => (tree (list 'x 0) '())
     
   (annotate-depth (tree "Hello" (list (tree "World" '())
                                       (tree "!" '()))))
   => (tree (list "Hello" 0)
            (list (tree (list "World" 1) '())
                  (tree (list "!" 1) '())))
     
   (annotate-depth (tree 3 (list (tree 5 (list (tree 7 '()))))))
   => (tree (list 3 0) (list (tree (list 5 1) (list (tree (list 7 2) '())))))
   
   (annotate-depth (tree (list 3 0)
                         (list (tree (list 5 1)
                                     (list (tree (list 7 2) '()))))))
   => (tree (list (list 3 0) 0)
            (list (tree (list (list 5 1) 1)
                        (list (tree (list (list 7 2) 2) '())))))])
(define (annotate-depth t0)
  (local
    [(define-docs (annotate-depth/a depth t)
       [Signature: {All X} Nat [Tree (list X Nat)] -> [Tree (list X Nat)]]
       [Purpose: #<<"
Pairs each element in the @tree with its level, where the root of the
tree has level @depth.
"
                 ]
       [Accumulator: depth : #<<"
The current depth of the tree - how many calls
to @tree-children are needed to get from @t0 to @t.
"
                     ])
     (define (annotate-depth/a depth t)
       (mk-tree (list (tree-value t) depth)
                (map (curry annotate-depth/a (add1 depth))
                     (tree-children t))))]
    (annotate-depth/a 0 t0)))