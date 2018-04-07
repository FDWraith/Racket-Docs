#lang racket

(provide typed-top
         typed-datum
         typed-app
         typed-define
         typed-struct
         begin-without-type-checking)

(require [for-syntax "error.rkt"
                     "subtype.rkt"
                     "struct.rkt"
                     "../utils.rkt"
                     syntax/parse
                     racket/promise
                     racket/function
                     racket/list]
         "use.rkt"
         "builtin.rkt")

#;(define-docs typed-top
    [Syntax: (typed-top . x:id)]
    [Semantics: #<<"
Like @#%top, but allows @x to be unbound and gives it its proper type
if it has the @allow-unbound? property.
"
                ])
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
(define-syntax (typed-app stx)
  (syntax-parse stx
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
                      (if (cons? sub-stxs) (rest sub-stxs) '())))

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
    [(_ fid:id (prm:id ...) ((~datum local) [sub-defs ...] body ... out))
     ; Splices local to allow type-checking
     #'(typed-named-λ fid (prm ...) sub-defs ... body ... out)]
    [(_ fid:id (prm:id ...) body ... out)
     #'(λ (prm ...)
         (assign-λ-param-types fid (prm ...))
         body ...
         (assert-λ-out-type fid out))]))

(define-syntax assign-λ-param-types
  (syntax-parser
    [(_ fid:id (prm:id ...))
     (unless (skip-type-check? #'fid)
       #'(assign-λ-param-types/force fid (prm ...)))]))

(define-syntax assign-λ-param-types/force
  (syntax-parser
    [(_ fid:id (prm:id ...))
     (define f-type (try-get-id-type #'fid))
     (define param-types (and f-type (try-func-params f-type)))
     (when (and f-type param-types)
       (for [(param-stx (syntax->list #'(prm ...)))
             (param-type param-types)]
         (add-id-type! param-stx param-type)))
     #'(void)]))

(define-syntax assert-λ-out-type
  (syntax-parser
    [(_ fid:id out)
     (unless (skip-type-check? #'fid)
       #'(assert-λ-out-type/force fid out))]))

(define-syntax assert-λ-out-type/force
  (syntax-parser
    [(_ fid:id out)
     (define f-type (try-get-id-type #'fid))
     (define out-type (and f-type (try-func-out f-type)))
     (cond
       [(or (not f-type)
            (not out-type))
        #'out]
       [else
        (define-values (out+-stx actual-out-type) (type-of #'out))
        (unless (type<? actual-out-type out-type)
          (raise-out-error (get-type-error actual-out-type out-type)
                           #'fid
                           #'out
                           this-syntax))
        out+-stx])]))

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

(define-syntax typed-struct
  (syntax-parser
    [(_ id:id [field:id ...] . rest)
     #:with (field-types ...) (map/stx (const #'Any) #'(field ...))
     #:with cstr-type #'[-> field-types ... Any]
     #'(begin
         (assign-type/id/parsed id cstr-type)
         (struct id [field ...] . rest))]))

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