#lang racket

(provide [struct-out eval-example]
         [struct-out interpret-data-example]
         [struct-out plain-data-example])

#;(define-data Example
  [: - (eval-example Syntax Syntax Syntax)
     - (interpret-data-example Syntax Desc)
     - (plain-data-example Syntax)]
  [Interpretation: #<<"
A documented example.
- (eval-example #'a #'b #'(a => b)) documents that a evaluates to b -
  when a and b are evaluated, they're equal. (#'(a => b) is just the source).
- (interpret-data-example #'a "b") documents that a is an example of the term
  in source code (e.g. if the term is a type, a is an instance), and that it's
  interpretation in English is b.
- (plain-data-example #'a) documents that a is a code example of the term in
  source code (e.g. if the term is a type, a is an instance).
"
                   ]
  [Examples:
   "5 + 7 = 12" <= (eval-example #'(+ 5 7) #'12)
   "(* 2 (+ 3 4)) encodes the math expression (2 * (3 + 4))" <=
   (interpret-data-example #'(* 2 (+ 3 4)) "2 * (3 + 4)")
   "9 is an instance of the type being documented" <= (plain-data-example "9")])
(struct eval-example [expr expected stx] #:transparent)
(struct interpret-data-example [expr interpretation] #:transparent)
(struct plain-data-example [expr] #:transparent)