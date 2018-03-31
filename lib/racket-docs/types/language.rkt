#lang racket

(provide typed-top
         typed-datum
         typed-app
         typed-define
         begin-without-type-checking)

(require [for-syntax "error.rkt"
                     "subtype.rkt"
                     "struct.rkt"
                     "../utils.rkt"
                     syntax/parse
                     racket/function
                     racket/list]
         "use.rkt"
         "builtin.rkt")

#;(define-docs typed-top
    )
(define-syntax typed-top
  (syntax-parser
    [(_ . x)
     (cond
       [(allow-unbound? #'x)
        (define type
          (λ () (or (try-get-id-type #'x)
                    (union '()))))
        (assign-type/stx/parsed #'(void) type)]
       [else #'(#%top . x)])]))

#;(define-docs typed-datum
    [Syntax: (typed-datum . x)]
    [Semantics: "Assigns the datum its proper type."]
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
  (define sub-stxs (remove-duplicates (map (curry list-ref x-stxs)
                                           (app-error-param-idxs err))))
  (raise-syntax-error (and (symbol? f-datum) f-datum)
                      (app-error-msg err)
                      full-stx
                      (and (cons? sub-stxs) (first sub-stxs))
                      (and (cons? sub-stxs) (rest sub-stxs))))

#;(define-docs typed-define
    [Syntax:
     (typed-define defd:id body ...+)
     (typed-define (defd:id prm:id ...) body ...+)]
    [Semantics: #<<"
If defining a function and the function's identifier has a type,
gives its parameters types and typechecks the output.
"
                ])
(define-syntax typed-define
  (syntax-parser
    [(_ defd:id body ...+)
     #'(define defd body ...)]
    [(_ (defd:id prm:id ...) body ...+)
     #'(define defd (typed-named-λ defd (prm ...) body ...))]))

#;(define-docs typed-named-λ
    [Syntax: (typed-named-λ fid:id (prm:id ...) body ... out)]
    [Semantics: #<<"
Based on the type of the function's identifier @fid,
assigns types to the arguments,
and raises a syntax error if the body doesn't conform to the output.
"
                ])
(define-syntax typed-named-λ
  (syntax-parser
    [(_ fid:id (prm:id ...) body ... out)
     (define-values (_ type) (type-of #'fid))
     (define param-types (try-func-params type))
     (define out-type (try-func-out type))
     (when param-types
       (for [(param-stx (syntax->list #'(prm ...)))
             (param-type param-types)]
         (add-id-type! param-stx param-type))
       ; Raises type errors (output type errors raised in (when out-type ...))
       (for-each type-of (syntax->list (allow-unbound #'(body ...)))))
     (when out-type
       (define-values (_ actual-out-type) (type-of (allow-unbound #'out)))
       (unless (type<? actual-out-type out-type)
         (raise-out-error (get-type-error actual-out-type out-type)
                          #'fid
                          #'out
                          this-syntax)))
     #'(λ (prm ...)
         body ...
         out)]))

#;(define-docs (raise-out-error err f-stx out-stx full-stx)
    [Signature: TypeError Syntax Syntax Syntax -> Syntax]
    [Purpose:
     "Raises a syntax error describing the function application error."])
(define-for-syntax (raise-out-error err f-stx out-stx full-stx)
  (define f-datum (syntax-e f-stx))
  (raise-syntax-error f-datum
                      (string-append
                       "Function output has the wrong type:\n"
                       (type-error-msg err))
                      full-stx
                      out-stx))

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
prevents the syntax from being type-checked.
"
                ])
  (define skip-type-check?-prop 'skip-type-check?)

  #;(define-docs allow-unbound?-prop
      [Signature: Symbol]
      [Purpose: #<<"
A syntax property key which, if true,
prevents unbound identifiers from automatically raising errors.
"
                ])
  (define allow-unbound?-prop 'allow-unbound?)

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
    (syntax-property/recur stx skip-type-check?-prop #true))

  #;(define-docs (allow-unbound? stx)
      [Signature: Syntax -> Bool]
      [Purpose: #<<"
Allow unbound identifiers (e.g. when getting function output types)?
"
                ])
  (define (allow-unbound? stx)
    (syntax-property stx allow-unbound?-prop))

  #;(define-docs (allow-unbound stx)
      [Signature: Syntax -> Syntax]
      [Purpose: #<<"
Returns a syntax identical to @stx, but unbound identifiers won't raise errors.
"
                ])
  (define (allow-unbound stx)
    (syntax-property/recur stx allow-unbound?-prop #true)))