#lang racket

(provide define-type
         assign-type

         ; Special types
         Union
         ->
         ; Primitive Types
         Nothing
         Bool
         Pos
         Nat
         Int
         Num
         String

         ; Built-In Non-Primitive Types
         Any
         Listof
         Maybe
         List)

(require [for-syntax syntax/parse
                     "utils.rkt"])

#;(define-data Type
  [: -> UnwrappedType]
  [Interpretation: #<<"
A type specified in the type system.
Some values are instances of the type, some aren't.
Types are wrapped in thunks so they can be recursive,
e.g. you can create the type LoS = (cons String LoS).
"
                   ])

#;(define-data UnwrappedType
  [: - PrimitiveType
     - UnionType
     - FunctionType
     - ExpressionType]
  [Interpretation: #<<"
A type specified in the type system.
Some values are instances of the type, some aren't.
Types are wrapped in thunks so they can be recursive,
e.g. you can create the type LoS = (cons String LoS).
This is what you get when you call the thunk.
"
                   ])

#;(define-data PrimitiveType
  [: (primitive)]
  [Interpretation: #<<"
A built-in type.
The the type's label is the identifier it's defined as,
e.g. the primitive type in (define-type String (primitive))
encodes the string type.

Every primitive type is disjoint from every other primitive type.
"
                   ]
  [Examples: (define-type String (primitive))])
(struct primitive [])

#;(define-data UnionType
  [: (union [Listof Type])]
  [Interpretation: "A union of types, created by [(~datum Union) A:type ...]."]
  [Examples:
   [Union Symbol Int String] => (union (list Symbol Int String))
   "A union of the types Symbol, Int, and String" <=
   (union (list Symbol Int String))])
(struct union [subs] #:transparent)

#;(define-data FunctionType
  [: (func [Listof Type] Type)]
  [Interpretation:
   "A function type, created by [(~datum ->) I:type ... O:type]."]
  [Examples:
   [-> Foo Bar Baz] => (func (list Foo Bar) Baz)
   "A function which takes a Foo and Bar, and returns a Baz." <=
   (func (list Foo Bar) Baz)])
(struct func [params out] #:transparent)

#;(define-data ExpressionType
  [: - Atom
     - [Listof Type]]
  [Interpretation: #<<"
A type defined by an expression.
A value is an instance iff it matches the shape of the type -
if all (lowercase) atoms (e.g. cons) are as-is in the value,
and all types are replaced by instances (see examples).
"
                   ]
  [Examples:
   "This type has 1 instance - the string \"foo\"" <= (expr "foo")
   #<<"
This would match (cons \"Hello\" \"World\") and (cons \"A\" \"B\"),
but not (cons \"Z\" 7), (foo \"?\" \"E\"), or 'alskdj.
"
             <= (expr `(cons ,String ,String))])

#;(define-docs (expr? x)
    [Signature: Type -> Bool]
    [Purpose: "Whether x is an expression type."]
    [Examples:
     (expr? String) => #false
     (expr? 'a) => #true
     (expr? `(a ,String) => #true)])
(define (expr? x)
  (or (symbol? x)
      (list? x)))

#;(define-docs define-type
    [Syntax: (define-type defd:id defn)]
    [Semantics: #<<"
Parses defn - e.g. makes it an expression type if lowercase.
Creates a new type, defd, which is equivalent to defn - whenever an instance of
defn is expected, an instance of defd can be provided, and vice versa.
If defd is a list, it creates a function type which takes the given parameters
(which MUST be valid type identifiers - they can't start with lowercase
characters) and replaces them in defn to get a result.
"
                ]
    [Examples:
     (define-type Integer Int)
     (define-type Int [Union Pos 0 (- Pos)])
     (define-type Listof-String [Union '() (cons String Listof-String)])])
(define-syntax (define-type stx)
  (syntax-parse stx
    [(_ defd:id defn)
     #:with defn+ (parse-type #'defn)
     #'(define-type/parsed+un defd (defn+))]
    [(_ (defd:id param:id ...) defn)
     #:with defn+ (parse-type #'defn)
     (define bad-param-stx
       (findf (compose not type-identifier?) (syntax->list #'(param ...))))
     (when bad-param-stx
       (raise-syntax-error 'define-type
                           "Param must be valid type identifier"
                           stx
                           bad-param-stx))
     #'(define-type/parsed+un (defd param ...) (defn+))]))

#;(define-docs define-type/primitive
    [Syntax: (define-type/primitive defd:id)]
    [Semantics: "Creates a new primitive type and defined defd to it."]
    [Examples: "Defines the type Bool." <= (define-type/parsed Bool)])
(define-syntax define-type/primitive
  (syntax-parser
    [(_ defd:id) #'(define-type/parsed+un defd (primitive))]))

#;(define-docs define-type/parsed+un
    [Syntax: (define-type/parsed+un defd:id defn)
             (define-type/parsed+un (defd:id param:id ...) defn ...)
             (define-type/parsed+un (defd:id param:id ... . rest:id) defn ...)]
    [Semantics: #<<"
Assumes defn is already parsed - e.g. it can be a primitive type.
Creates a new type, defd, which is equivalent to defn - whenever an instance of
defn is expected, an instance of defd can be provided, and vice versa.
If defd is a list, it creates a function type which takes the given parameters
and creates a result using (defn ...).
"
                ]
    [Examples:
     (define-type Integer (primitive))
     (define-type Int [Union Pos 0 (- Pos)])])
(define-syntax define-type/parsed+un
  (syntax-parser
    [(_ defd:id defn)
     #'(define defd
         (λ () defn))]
    [(_ (defd:id param:id ...) defn ...)
     #'(define (defd param ...)
         (λ () defn ...))]
    [(_ (defd:id param:id ... . rest:id) defn ...)
     #'(define (defd param ... . rest)
         (λ () defn ...))]))

#;(define-docs assign-type
    [Syntax: (assign-type val:id type)]
    [Semantics: #<<"
Parses type - e.g. makes it an expression type if lowercase.
Specifies that val is an instance of type.
If the compiler knows that val isn't an instance of type,
this will raise an error.
Otherwise, in other cases, the compiler will assume that val
is an instance of type.
"
                ]
    [Examples:
     (assign-type "Hello" String)
     (assign-type 7 [Union Pos 0 (- Pos)])])
(define-syntax assign-type
  (syntax-parser
    [(_ val:id type)
     #:with type+ (parse-type #'type)
     #'(assign-type/parsed val type+)]))

#;(define-docs assign-type/parsed
    [Syntax: (assign-type/parsed val:id type)]
    [Semantics: #<<"
Assumes type is already parsed - e.g. it can be a primitive type.
Specifies that val is an instance of type.
If the compiler knows that val isn't an instance of type,
this will raise an error.
Otherwise, in other cases, the compiler will assume that val
is an instance of type.
"
                ]
    [Examples:
     (assign-type/parsed "Hello" String)
     (assign-type/parsed 7 (union (list Pos 0 (- Pos))))])
(define-syntax assign-type/parsed
  (syntax-parser
    [(_ val:id type)
     ; TODO Actually assign the type - make it a syntax property?
     #'(printf "Assigned type: ~a app= ~a\n" type (type))]))

; TODO parse-type - Parses the type.
; Converts every lowercase identifier into a symbol,
; converts every s-list starting with a quoted into a regular list.
; Definitely don't local expand.
(begin-for-syntax
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
       (parse-type #'foo) => #''foo
       (parse-type #'(Union A b C)) => #'(Union A 'b C)
       (parse-type #'(cons x Y)) => #'`(cons x ,Y)])
  (define parse-type
    (syntax-parser
      [x:id
       (cond
         [(type-identifier? #'x) #'x]
         [else #''x])]
      ['x #''x]
      [(head param ...)
       #:with head+ (parse-type #'head)
       #:with (param+ ...) (map/stx parse-type #'(param ...))
       (cond
         [(expr-stx? #'head+) #'(list head+ param+ ...)]
         [else #'(head+ param+ ...)])]
      [x #'x])) ; Numbers, booleans, etc. All expression types.

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
      ['_ #true]
      [((~literal list) _ ...) #true]
      [_ #false])))

#;(define-docs [Union . Xs]
    [Signature: [Type ... -> UnionType]]
    [Semantics: "Creates a union type, with the given parameters as sub-types."]
    [Examples: [Union String Bool] => (union (list String Bool))])
(define-type/parsed+un (Union . xs)
  (union xs))

#;(define-docs [-> X . Xs]
    [Signature: [Type ... -> FunctionType]]
    [Semantics: #<<"
Creates a function type, with all but the last parameter as
param types, and the last parameter as the output type.
"
                ]
    [Examples: [-> String Bool] => (func (list String) Bool)])
(define-type/parsed+un (-> arg . rest-args)
  (define args (cons arg rest-args))
  (define-values (params out) (split-at-right args 1))
  (func params out))

(define-type/primitive Nothing)
(define-type/primitive Bool)
(define-type/primitive Pos)
(define-type/primitive Nat)
(define-type/primitive Int)
(define-type/primitive Num)
(define-type/primitive String)
(define-type Any [Union])
(define-type [Listof X]
  (Union '() (cons X [Listof X])))
(define-type [Maybe X]
  (Union #false X))
(define-type List [Listof Any])