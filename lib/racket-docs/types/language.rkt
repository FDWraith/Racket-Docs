#lang racket

(provide typed-datum
         typed-app
         begin-without-type-checking)

(require [for-syntax "error.rkt"
                     "subtype.rkt"
                     "struct.rkt"
                     "../utils.rkt"
                     syntax/parse
                     racket/function]
         "use.rkt"
         "builtin.rkt")

#;(define-docs typed-datum
    [Syntax: (typed-datum . x)]
    [Semantics: #<<"
Assigns the datum its proper type.
"
                ]
    [Examples: #'(typed-datum . 5) => (assign-type #'5 Integer)])
(define-syntax typed-datum
  (syntax-parser
    [(_ . x) (assign-type/stx/parsed #'(#%datum . x) (datum-type #'x))]))

#;(define-docs (datum-type stx)
    [Signature: Syntax -> Type]
    [Semantics: "The primitive type of the datum."])
(define-for-syntax datum-type
  (syntax-parser
    [_:boolean Bool]
    [_:exact-positive-integer PosInt]
    [_:nat Nat]
    [_:exact-integer NegInt]
    [_:integer Int]
    [_:number Decimal]
    [_:char Char]
    [_:string String]
    [_ Unknown]))

#;(define-docs typed-app
    [Syntax: (typed-app f x ...)]
    [Semantics: #<<"
Throws an error if the function arguments definately don't conform to the
function's type.
"
                ])
(define-syntax typed-app
  (syntax-parser
    [(_ f x ...)
     (cond
       [; Checks only the function, because this-syntax or #'(f x ...)
        ; wraps in syntax without the property.
        (skip-type-check? #'f) #'(#%app f x ...)]
       [else
        (define x-stx-list (syntax->list #'(x ...)))
        (define-values (f+-stx f-type) (type-of #'f))
        (define-values (x+-stx-list x-types)
          (map/unzip type-of 2 x-stx-list))
        (define x+-stxs (datum->syntax #'(x ...) x+-stx-list))
        (unless (params<? x-types f-type)
          (raise-app-error (get-app-error x-types f-type)
                           #'f
                           x-stx-list
                           this-syntax))
        (with-syntax [(f+ f+-stx)
                      ((x+ ...) x+-stxs)]
          (define out-gen-type
            (or (map/maybe try-func-out
                           (refine-for-params x-types f-type)) Unknown))
          (define out-shape-type (λ () (cons f-type x-types)))
          (define out-type [Intersection out-gen-type out-shape-type])
          (assign-type/stx/parsed #'(#%app f+ x+ ...) out-type))])]))

#;(define-docs (raise-app-error err f-stx x-stxs full-stx)
    [Signature: ParamError [Listof Syntax] Syntax -> Syntax]
    [Purpose:
     "Raises a syntax error describing the function application error."])
(define-for-syntax (raise-app-error err f-stx x-stxs full-stx)
  (define f-datum (syntax-e f-stx))
  (apply raise-syntax-error
         (and (symbol? f-datum) f-datum)
         (app-error-msg err)
         full-stx
         (map (curry list-ref x-stxs)
              (app-error-param-idxs err))))

#;(define-docs (begin-without-type-checking)
    [Syntax: (begin-without-type-checking expr ...)]
    [Semantics: #<<"
Equivalent to (begin expr ...), but expressions inside this block
aren't type checked. Specifically, applying a function to values with the wrong
types won't cause a type error.
"
              ])
(define-syntax begin-without-type-checking
  (syntax-parser
    [(_ expr ...)
     #:with (expr+ ...) (skip-type-check #'(expr ...))
     #'(begin expr+ ...)]))

(begin-for-syntax
  #;(define-docs skip-type-check?-prop
      [Signature: Symbol]
      [Purpose: #<<"
A syntax property key which, if true,
prevents the syntax from being type-checked."
"
                ])
  (define skip-type-check?-prop 'skip-type-check?)

  #;(define-docs (skip-type-check? stx)
      [Signature: Syntax -> Bool]
      [Purpose: "Don't type check this syntax?"])
  (define (skip-type-check? stx)
    (syntax-property stx skip-type-check?-prop))

  #;(define-docs (skip-type-check stx)
      [Signature: Syntax -> Syntax]
      [Purpose: #<<"
Returns a syntax identical to @stx, but it won't be type checked.
"
                ])
  (define (skip-type-check stx)
    (syntax-property/recur stx skip-type-check?-prop #true)))