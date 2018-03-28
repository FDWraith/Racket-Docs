#lang racket

(provide [struct-out labeled-type]
         [struct-out primitive]
         [struct-out intersection]
         [struct-out union]
         [struct-out func]
         Intersection/parsed
         Union/parsed
         ->/parsed
         expr?
         try-func-out
         type-summary
         type-label)

#;(define-data Type
  [: - (labeled-type [-> UnwrappedType] String)
     - -> UnwrappedType]
  [Interpretation: #<<"
A type specified in the type system.
Some values are instances of the type, some aren't.
Types are wrapped in thunks so they can be recursive,
e.g. you can create the type LoS = (cons String LoS).
"
                   ])
(struct labeled-type [proc label]
  #:property prop:procedure (struct-field-index proc))

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

#;(define-docs summary-recur-limit
    [Signature: Nat]
    [Purpose: #<<"
How many layers of sub-types will be used in a type summary or label,
before ... is used. Prevents unnecessary or infinite recursion.
"
              ])
(define summary-recur-limit 10)

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
  (not (or (primitive? x)
           (intersection? x)
           (union? x)
           (func? x))))

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
    [Purpose: "A detailed summary of the type, useful for debugging."]
    [Examples:
     (type-summary (λ () (primitive "String"))) => "String"
     (type-summary Nat) => "[Intersection Pos '0]"])
(define (type-summary type)
  (define (type-summary/acc type recurs-left)
    (cond
      [(zero? recurs-left) "..."]
      [else
       (type-summary/recur type (const #true)
                           (curryr type-summary/acc (sub1 recurs-left)))]))
  (type-summary/acc type summary-recur-limit))

#;(define-docs (type-label type)
    [Signature: Type -> String]
    [Purpose: "A brief label of the type, useful for displaying to the user."]
    [Examples:
     (type-label (λ () (primitive "String"))) => "String"
     (type-label Nat) => "Nat"])
(define (type-label type)
  (define (type-label/acc type recurs-left)
    (cond
      [(labeled-type? type) (labeled-type-label type)]
      [(zero? recurs-left) "..."]
      [else (type-summary/recur type (λ (x) (not (expr? (x))))
                                (curryr type-label/acc (sub1 recurs-left)))]))
  (type-label/acc type summary-recur-limit))

#;(define-docs (type-summary/recur type sub-filter sub-summary)
    [Signature: Type [Type -> Bool] [Type -> String] -> String]
    [Purpose: #<<"
A summary of the type. Ignores intersection sub-types which return false in
@sub-filter (pretends that they don't exist), and summarizes sub-types via
@sub-summary.
"
              ]
    [Examples:
     (type-summary/recur (λ () (primitive "String")) (λ (x) "foo")) => "String"
     (type-summary/recur Nat (λ (x) "foo")) => "[Intersection foo foo]"])
(define (type-summary/recur type sub-filter sub-summary)
  (define type+ (type))
  (cond
    [(primitive? type+) (primitive-label type+)]
    [(intersection? type+)
     (define intersection-subs* (filter sub-filter (intersection-subs type+)))
     (cond
       [(empty? intersection-subs*) "Any"]
       [(empty? (rest intersection-subs*))
        (sub-summary (first intersection-subs*))]
       [else
         (format "[Intersection ~a]"
                 (string-join (map sub-summary intersection-subs*) " "))])]
    [(union? type+)
     (cond
       [(empty? (union-subs type+)) "Nothing/Unknown"]
       [(empty? (rest (union-subs type+)))
        (sub-summary (first (union-subs type+)))]
       [else
        (format "[Union ~a]"
                (string-join (map sub-summary (union-subs type+)) " "))])]
    [(func? type+)
     (format "[~a -> ~a]"
             (string-join (map sub-summary (func-params type+)) " ")
             (sub-summary (func-out type+)))]
    [(list? type+)
     (format "(~a)"
             (string-join (map sub-summary type+) " "))]
    [else (format "~v" type+)]))