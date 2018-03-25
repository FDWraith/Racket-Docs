#lang racket

(provide [for-syntax assign-type/stx
                     assign-type/stx/parsed
                     type-of]
         assign-type/id
         assign-type/id/parsed)

(require [for-syntax [for-syntax "parse.rkt"
                                 syntax/parse
                                 racket/base]
                     "parse.rkt"
                     "../utils.rkt"
                     [prefix-in mt. "macrotypes/typecheck.rkt"]
                     syntax/parse
                     racket/syntax
                     racket/match
                     racket/list]
         "builtin.rkt"
         "struct.rkt")

(begin-for-syntax
  #;(define-docs cur-id-types
      [Signature: [Listof (cons Identifier Type)]]
      [Purpose: "Types associated with identifiers."])
  (define cur-id-types '())
  
  #;(define-docs assign-type/stx
      [Syntax: (assign-type/stx stx type)]
      [Semantics: #<<"
Parses type - e.g. makes it an expression type if lowercase.
Specifies that value in syntax is an instance of type.
If val is ever used in a situation where it should be another type,
the compiler will raise an error.
"
                  ]
      [Examples:
       (assign-type/stx #'"Hello" String)
       (assign-type/stx #'7 [Union Pos 0 (- Pos)])])
  (define-syntax assign-type/stx
    (syntax-parser
      [(_ stx type)
       #:with type+ (parse-type #'type)
       #'(assign-type/stx/parsed stx type+)]))

  #;(define-docs assign-type/stx/parsed
      [Signature: Syntax Type -> Syntax]
      [Purpose: #<<"
Assumes type is already parsed - e.g. it can be a primitive type.
Specifies that value in syntax is an instance of type.
If val is ever used in a situation where it should be another type,
the compiler will raise an error.
"
                ]
      [Examples:
       (assign-type/stx/parsed #'"Hello" String)
       (assign-type/stx/parsed #'7 [Union Pos 0 (- Pos)])])
  (define assign-type/stx/parsed mt.assign-type)

  #;(define-docs type-of
      [Signature: Syntax -> Syntax Type]
      [Purpose: #<<"
Gets the type of value encoded in the syntax.
Also expands the syntax.
"
                ]
      [Examples:
       (type-of #'"Hello") => (values #'"Hello" String)
       (type-of #'foo)
       (type-of #'(- 1 2)) => (values #'(- 1 2) Int)
       (type-of #'(cons foo 4)) =>
       (values #'(cons foo 4) `(cons ,foo.type ,Nat))])
  (define (type-of stx)
    (define-values (stx+ gen-type) (gen-type-of stx))
    (define-values (stx++ shape-type) (shape-type-of stx+))
    (values stx++ [Intersection gen-type shape-type]))

  #;(define-docs shape-type-of
      [Signature: Syntax -> Term Type]
      [Purpose: #<<"
Gets the literal type of the value encoded in the syntax.
Also may expand sub-terms in the syntax, because it needs to get their types.
"
                ]
      [Examples:
       (shape-type-of #'foo) => (values #'foo (λ () 'foo))
       (shape-type-of #'(cons 0 1)) =>
       (values #'(cons 0 1)
               (λ () (list (type-of #'cons) (type-of #'0) (type-of #'1))))])
  (define (shape-type-of stx)
    (define x (syntax-e stx))
    (cond
      [(and (list? x)
            (cons? x)
            (equal? (syntax-e (first x)) 'quote))
       ; "Over-expanded" datum - e.g. 7 gets expanded into '7.
       (shape-type-of (second x))]
      [(list? x)
       (define-values (sub-stxs sub-types)
         (map/unzip type-of 2 x))
       (values (datum->syntax stx sub-stxs)
               (λ () sub-types))]
      [else (values stx (λ () x))]))
  
  #;(define-docs gen-type-of
      [Signature: Syntax -> Syntax Type]
      [Purpose: #<<"
Gets the type of value encoded in the syntax,
which doesn't include the literal type itself.
Also expands the syntax.
"
                ]
      [Examples:
       (gen-type-of #'"Hello") => (values #'"Hello" String)
       (gen-type-of #'foo)
       (gen-type-of #'(- 1 2)) => (values #'(- 1 2) Int)
       (gen-type-of #'(cons foo 4)) =>
       (gen-type-of #'(cons foo 4) `(cons ,foo.type ,Nat))])
  (define (gen-type-of stx)
    (match-define (list stx+ explicit-type) (mt.type-of stx))
    (define type
      (or explicit-type
          (and (identifier? stx+)
               (try-get-id-type stx+))
          Unknown))
    (values stx+ type))
  
  #;(define-docs (try-get-id-type id)
      [Signature: Identifier -> [Maybe Type]]
      [Purpose: #<<"
Gets the type of value referenced by the identifier,
if the identifier is specifically defined to refer to the value.
"
                ]
      [Examples: (try-get-id-type #'foo)])
  (define (try-get-id-type id)
    (map/maybe cdr (assoc id cur-id-types free-identifier=?)))

  #;(define-docs (add-id-type id type)
      [Signature: Identifier Type -> Void]
      [Purpose: "Assigned the type to the identifier via cur-id-types."]
      [Examples: (add-id-type #'foo String)]
      [Effect: "Adds an entry to cur-id-types."])
  (define (add-id-type id type)
    (set! cur-id-types (cons (cons id type) cur-id-types))))

#;(define-docs assign-type/id
    [Syntax: (assign-type/id val:id type)]
    [Semantics: #<<"
Parses type - e.g. makes it an expression type if lowercase.
Specifies that the identifier is an instance of type.
If val is ever used in a situation where it should be another type,
the compiler will raise an error.
"
                ]
    [Examples:
     (assign-type "Hello" String)
     (assign-type 7 [Union Pos 0 (- Pos)])])
(define-syntax assign-type/id
  (syntax-parser
    [(_ val:id type)
     #:with type+ (parse-type #'type)
     #'(assign-type/id/parsed val type+)]))

#;(define-docs assign-type/id/parsed
    [Syntax: (assign-type/id/parsed val:id type)]
    [Semantics: #<<"
Assumes type is already parsed - e.g. it can be a primitive type.
Specifies that the identifier is an instance of type.
If val is ever used in a situation where it should be another type,
the compiler will raise an error.
"
                ]
    [Examples:
     (assign-type/id/parsed "Hello" String)
     (assign-type/id/parsed 7 (union (list Pos 0 (- Pos))))])
(define-syntax assign-type/id/parsed
  (syntax-parser
    [(_ val:id type)
     #'(begin-for-syntax
         (add-id-type #'val type))]))