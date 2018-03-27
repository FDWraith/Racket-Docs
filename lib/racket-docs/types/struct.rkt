#lang racket

(provide [struct-out primitive]
         [struct-out intersection]
         [struct-out union]
         [struct-out func]
         Intersection/parsed
         Union/parsed
         ->/parsed
         expr?
         try-func-out
         type-summary
         basic-type-summary)

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
     - IntersectionType
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
  [: (primitive String)]
  [Interpretation: #<<"
A built-in type.
The primtive's label should always be its identifier,
e.g. the primitive for Foo would be (primitive "Foo").

Every primitive type is disjoint from every other primitive type.
"
                   ]
  [Examples:
   (define-type Foo (primitive "Foo"))
   (define-type String (primitive "String"))])
(struct primitive [label] #:transparent)

#;(define-data IntersectionType
  [: (union [Listof Type])]
  [Interpretation: #<<"
An intersection of types, created by [(~datum Intersection) A:type ...].
A value is an instance of this type if it's an instance of all sub-types.
"
                   ]
  [Examples:
   [Intersection Int Nat] => (intersection (list Int Nat))
   "An intersection of the types Int and Nat" <=
   (intersection (list Int Nat))])
(struct intersection [subs] #:transparent)

#;(define-data UnionType
  [: (union [Listof Type])]
  [Interpretation: #<<"
A union of types, created by [(~datum Union) A:type ...].
A value is an instance of this type if it's an instance of any sub-types.
"
                   ]
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

#;(define-docs (Intersection/parsed xs)
    [Signature: Type -> Type]
    [Semantics: #<<"
Creates an intersection type, with the given parameters as sub-types.
A value belongs to this type if it belongs to all of the sub-types.
Unlike regular Intersection, doesn't parse the given types.
"
                ]
    [Examples:
     [Intersection String Bool] => (λ () (intersection (list String Bool)))])
(define (Intersection/parsed xs)
  (λ () (intersection xs)))

#;(define-docs (Union/parsed xs)
    [Signature: Type -> Type]
    [Semantics: #<<"
Creates a union type, with the given parameters as sub-types.
A value belongs to this type if it belongs to any of the sub-types.
Unlike regular Union, doesn't parse the given types.
"
                ]
    [Examples:
     (Union/parsed (list String Bool)) => (λ () (union (list String Bool)))])
(define (Union/parsed xs)
  (λ () (union xs)))

#;(define-docs (->/parsed params out)
    [Signature: [Listof Type] Type -> Type]
    [Semantics: #<<"
Creates a function type, with the types in the first parameter as
param types, and the second parameter as the output type.
Unlike regular ->, doesn't parse the given types.
"
                ]
    [Examples:
     (->/parsed (list String) Bool) => (λ () (func (list String) Bool))])
(define (->/parsed params out)
  (λ () (func params out)))

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

#;(define-docs (try-func-out x)
    [Signature: Type -> [Maybe Type]]
    [Purpose: "@f's output if it's function type, otherwise #false."]
    [Examples:
     (try-func-out (func (list Int Nat) String)) => String
     (try-func-out String) => Any])
(define (try-func-out x)
  (define x+ (x))
  (cond
    [(func? x+) (func-out x+)]
    [(intersection? x+)
     (λ () (intersection (filter-map try-func-out (intersection-subs x+))))]
    [(union? x+)
     (define sub-outs (map try-func-out (union-subs x+)))
     (and (not (member #false sub-outs))
          (λ () (union sub-outs)))]
    [else #false]))

#;(define-docs (type-summary type)
    [Signature: Type -> String]
    [Purpose: "A summary of the type, for debugging."]
    [Examples:
     (type-summary (primitive "String")) => "String"
     (type-summary (intersection (list (primitive "Nat") 0)) =>
                   "[Intersection Nat '0]")])
(define (type-summary type)
  (define type+ (type))
  (cond
    [(primitive? type+) (primitive-label type+)]
    [(intersection? type+)
     (format "[Intersection ~a]"
             (string-join (map type-summary (intersection-subs type+)) " "))]
    [(union? type+)
     (format "[Union ~a]"
             (string-join (map type-summary (union-subs type+)) " "))]
    [(func? type+)
     (format "[~a -> ~a]"
             (string-join (map type-summary (func-params type+)) " ")
             (type-summary (λ () (func-out type+))))]
    [(list? type+)
     (format "(~a)"
             (string-join (map type-summary type+) " "))]
    [else (format "'~a" type+)]))

#;(define-docs (basic-type-summary type)
    [Signature: Type -> [Listof String]]
    [Purpose: "A summary of the surface layer of the type"]
    [Examples:
     (basic-type-summary (primitive "String")) => "String"
     (basic-type-summary (list (primitive "Nat"))) => "[Listof Nat]"
     (basic-type-summary
      (union (primitive "Nat")
             (list (union (primitive "Nat")
                          (primitive "PosInt"))
                   (intersection (primitive "Int") 0)))) =>
     (list "Nat" "[Listof [Union PosInt [Intersection Int '0]]]")])
(define (basic-type-summary type)
  (define type+ (type))
  (println type+)
  (cond
    [(union? type+) (map type-summary (union-subs type+))]
    [else (list (type-summary type))]))