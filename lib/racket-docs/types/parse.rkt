#lang racket

(provide parse-type
         type-identifier?)

(require syntax/parse
         "../utils.rkt"
         [for-template racket/base])

#;(define-docs parse-type
    [Signature: Syntax -> [Stx Type]]
    [Purpose: #<<"
Converts the value into a literal type (inside the syntax).
Converts every identifier starting with a lowercase character into a symbol
(expression type).
Converts every s-list starting with an expression type into a regular list
(also expression type).
Leaves identifiers not starting with lowercase characters as-is
(they refer to existing types).
"
              ]
    [Examples:
     (parse-type #'String) => #'String
     (parse-type #'foo) => #'(λ () 'foo)
     (parse-type #'[Union A b C]) => #'[Union A (λ () 'b) C]
     (parse-type #'(cons x Y)) => #'(λ () (list (λ () 'cons) (λ () 'x) Y))])
(define (parse-type stx)
  (syntax-parse stx
    [x:id
     (cond
       [(type-identifier? #'x) #'x]
       [else #'(λ () 'x)])]
    [((~datum quote) x) #'(λ () 'x)]
    [(head param ...)
     #:with head+ (parse-type #'head)
     #:with (param+ ...) (map/stx parse-type #'(param ...))
     (cond
       [(expr-stx? #'head) #'(λ () (list head+ param+ ...))]
       [else #'(head+ param+ ...)])]
    [x #'(λ () x)])) ; Numbers, booleans, etc. All expression types.

#;(define-docs (type-identifier? id-stx)
    [Signature: Identifier -> Bool]
    [Purpose: #<<"
Whether x directly refers to type - specifically,
whether x doesn't start with a lowercase character.
Otherwise, it will be parsed into an expression type.
"
              ]
    [Examples:
     (type-identifier? #'String) => #true
     (type-identifier? #'foo) => #false])
(define (type-identifier? id-stx)
  (not (char-lower-case? (string-ref (symbol->string (syntax-e id-stx)) 0))))

#;(define-docs (expr-stx? stx)
    [Signature: Syntax -> Bool]
    [Purpose: "Whether the syntax encodes an expression type."]
    [Examples:
     (expr-stx? #'String) => #false
     (expr-stx? #''foo) => #true
     (expr-stx? #'[-> Int String]) => #false
     (expr-stx? #'`(a b ,Int)) => #true])
(define expr-stx?
  (syntax-parser
    [x:id (not (type-identifier? #'x))]
    [((~datum quote) x) #true]
    [(head param ...) (expr-stx? #'head)]
    [_ #true]))