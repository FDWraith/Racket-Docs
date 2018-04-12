#lang racket

(provide [struct-out eval-example]
         [struct-out interpret-data-example]
         [struct-out plain-data-example]
         example-stx)

#;(define-data Example
  [: - (eval-example Syntax Syntax Syntax)
     - (interpret-data-example Syntax Desc [Maybe [Stx Type]])
     - (plain-data-example Syntax [Maybe [Stx Type]])]
  [Interpretation: #<<"
A documented example.
- @(eval-example #'a #'b #'(a => b) check?) documents that @a evaluates to @b -
  when @a and @b are evaluated, they're equal. (@#'(a => b) is just the source).
- @(interpret-data-example #'a "b" type) documents that @a is an instance of the
  type being documented (@type if provided), and that it's interpretation in
  English is @b.
- @(plain-data-example #'a type) documents that @a is an instance of the type
  being documented (@type if provided).
"
                   ]
  [Examples:
   "5 + 7 = 12" <= (eval-example #'(+ 5 7) #'12 #'[(+ 5 7) => 12])
   "(* 2 (+ 3 4)) encodes the math expression (2 * (3 + 4))" <=
   (interpret-data-example #'(* 2 (+ 3 4)) "2 * (3 + 4)" #'MathExpr)
   "9 is an instance of an integer (assuming an integer is being documented)" <=
   (plain-data-example #'9 #'Int)])
(struct eval-example [expr expected stx] #:transparent)
(struct interpret-data-example [expr interpretation type] #:transparent)
(struct plain-data-example [expr type] #:transparent)

#;(define-docs (example-stx example)
    [Signature: Example -> Syntax]
    [Purpose: #<<"
The syntax the example came from.
It should be highlighted if the example fails.
"
              ]
    [Examples:
     (example-stx (eval-example #'(+ 5 7) #'12 #'[(+ 5 7) => 12])) =>
     #'[(+ 5 7) => 12]
     (example-stx (interpret-data-example #'(* 2 (+ 3 4)) "2 * (3 + 4)")) =>
     #'(* 2 (+ 3 4))
     (example-stx (plain-data-example #'4)) => #'4])
(define (example-stx example)
  (cond
    [(eval-example? example) (eval-example-stx example)]
    [(interpret-data-example? example) (interpret-data-example-expr example)]
    [(plain-data-example? example) (plain-data-example-expr example)]))