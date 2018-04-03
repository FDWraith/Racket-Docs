#lang racket

(provide [for-syntax assign-type/stx
                     assign-type/stx/parsed
                     add-id-type!
                     type-of
                     try-get-id-type]
         assign-type/id
         assign-type/id/parsed
         assign-type/id/parsed/src
         define-typed-prim
         define-typed-prim/parsed)

(require [for-syntax [for-syntax "parse.rkt"
                                 syntax/parse
                                 racket/base]
                     "parse.rkt"
                     "../utils.rkt"
                     [prefix-in mt. "macrotypes/typecheck.rkt"]
                     syntax/parse
                     racket/syntax
                     racket/match
                     racket/string
                     racket/list]
         "builtin.rkt"
         "struct.rkt"
         racket/block)

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

  #;(define-docs (add-id-type! id type)
      [Signature: Identifier Type -> Void]
      [Purpose: "Assigns the type to the identifier via cur-id-types."]
      [Examples: (add-id-type! #'foo String)]
      [Effect: "Adds an entry to cur-id-types."])
  (define (add-id-type! id type)
    (set! cur-id-types (cons (cons id type) cur-id-types)))

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
    (define shape-type (shape-type-of stx+))
    (define type
      (if shape-type
          [Intersection gen-type shape-type]
          gen-type))
    (values stx+ type))

  #;(define-docs shape-type-of
      [Signature: Syntax -> [Maybe Type]]
      [Purpose: #<<"
Gets the literal type of the value encoded in the syntax.
If the syntax is a function application (list) or pair, returns #false,
because it should have a literal type assigned if it would be used.
"
                ]
      [Examples:
       (shape-type-of #'foo) => (λ () 'foo)
       (shape-type-of #'(cons 0 1)) => #false])
  (define (shape-type-of stx)
    (define x (syntax-e stx))
    (cond
      [(identifier? stx) (λ () (identifier-binding-symbol stx))]
      [(cons? x)
       (if (equal? (syntax-e (car x)) 'quote)
           ; "Over-expanded" datum - e.g. 7 gets expanded into '7.
           (shape-type-of (second x))
           #false)]
      [else (λ () x)]))
  
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
    (match-define (list stx+ explicit-type*) (mt.type-of stx))
    ; Fixes syntax property bug.
    (define explicit-type
      (if (and explicit-type* (procedure? (explicit-type*)))
          (explicit-type*)
          explicit-type*))
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
    (map/maybe cdr (assoc id cur-id-types free-identifier=?))))

#;(define-docs assign-type/id
    [Syntax: (assign-type/id val:id type)]
    [Semantics: #<<"
Parses @type - e.g. makes it an expression type if lowercase.
Specifies that the identifier @val is an instance of type.
If @val is ever used in a situation where it should be another type,
the compiler will raise an error.
"
                ]
    [Examples:
     (let [(foo 1)] (assign-type foo String)) => (void)
     (let [(bar 2)] (assign-type bar [Union Pos 0 (- Pos)])) => (void)])
(define-syntax assign-type/id
  (syntax-parser
    [(_ val:id type)
     #:with type+ (parse-type #'type)
     #'(assign-type/id/parsed val type+)]))

#;(define-docs assign-type/id/parsed
    [Syntax: (assign-type/id/parsed val:id type)]
    [Semantics: #<<"
Assumes @type is already parsed - e.g. it can be a primitive type.
Specifies that the identifier @val is an instance of type.
If @val is ever used in a situation where it should be another type,
the compiler will raise an error.
"
                ]
    [Examples:
     (let [(foo 1)] (assign-type/id/parsed foo String)) => (void)
     (let [(bar 2)]
       (assign-type/id/parsed bar (union (list Pos 0 (- Pos))))) => (void)])
(define-syntax assign-type/id/parsed
  (syntax-parser
    [(_ val:id type)
     #'(assign-type/id/parsed/src val type type)]))

#;(define-docs assign-type/id/parsed/src
    [Syntax: (assign-type/id/parsed/src val:id type src)]
    [Semantics: #<<"
Assumes @type is already parsed - e.g. it can be a primitive type.
Specifies that the identifier is an instance of type.
If @val is ever used in a situation where it should be another type,
the compiler will raise an error.
Uses @src when evaluating @type fails.
"
                ]
    [Examples:
     (let [(foo 1)]
       (assign-type/id/parsed/src foo String foo)) => (void)
     (let [(foo 1)
           (bar 2)]
       (assign-type/id/parsed/src bar (union (list Pos 0 (- Pos))) foo)) =>
     (void)])
(define-syntax assign-type/id/parsed/src
  (syntax-parser
    [(_ val:id type src)
     (with-handlers
         [(exn:fail? (λ (exn) (raise-syntax-error 'assign-type/id/parsed
                                                  (exn-message exn)
                                                  this-syntax
                                                  #'src)))]
       (add-id-type! #'val (eval-syntax #'type)))
     ; define-syntax allows for usage in local and binding arrows.
     #'(define-syntax foo type)]))


#;(define-docs define-typed-prim
    [Syntax:
     (define-typed-prim defd:id type)
     (define-typed-prim defd:id defn type)]
    [Semantics: #<<"
Defines @defd to a syntax transformer which expands to @defn with @type.
If @defn isn't provided (there are only 2 terms), it's inferred to be @un:defd
in @defd's scope.
"
                ]
    [Examples:
     (define-typed-prim cons+ cons [Forall X [-> X [Listof X] [Listof X]]])
     (define-typed-prim 7+ 7 7)])
(define-syntax define-typed-prim
  (syntax-parser
    [(_ defd:id defn type)
     #:with type+ (parse-type #'type)
     #'(define-typed-prim/parsed defd defn type+)]
    [(_ defd:id type)
     #:with defn (format-id #'defd "un.~a" #'defd #:source #'defd)
     #:with type+ (parse-type #'type)
     #'(define-typed-prim/parsed defd defn type+)]))

#;(define-docs define-typed-prim/parsed
    [Syntax: (define-typed-prim/parsed defd:id defn type)]
    [Semantics: #<<"
Assumes type is already parsed - e.g. it can be a primitive type.
Defines @defd to a syntax transformer which expands to @defn with @type.
"
                ]
    [Examples:
     (define-typed-prim/parsed cons+ cons
       [Forall X [-> X [Listof X] [Listof X]]])
     (define-typed-prim/parsed 7+ 7 (λ () 7))])
(define-syntax define-typed-prim/parsed
  (syntax-parser
    [(_ defd:id defn type)
     #:with defn+ #'(assign-type/stx/parsed #'defn type)
     #'(define-syntax defd (mk-applied-id-macro defn+))]))