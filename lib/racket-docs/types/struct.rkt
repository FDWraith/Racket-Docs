#lang racket

(provide [struct-out labeled-type]
         [struct-out primitive]
         [struct-out intersection]
         [struct-out union]
         [struct-out func]
         [struct-out forall]
         Intersection/parsed
         Union/parsed
         ->/parsed
         Forall/parsed
         expr?
         try-func-out
         unparameterize
         app-forall
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
     - ForallType
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
   ([Intersection Int Nat]) => (intersection (list Int Nat))
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
   ([Union Symbol Int String]) => (union (list Symbol Int String))
   "A union of the types Symbol, Int, and String" <=
   (union (list Symbol Int String))])
(struct union [subs] #:transparent)

#;(define-data FunctionType
  [: (func [Listof Type] Type)]
  [Interpretation:
   "A function type, created by [(~datum ->) I:type ... O:type]."]
  [Examples:
   ([-> Foo Bar Baz]) => (func (list Foo Bar) Baz)
   "A function which takes a Foo and Bar, and returns a Baz." <=
   (func (list Foo Bar) Baz)])
(struct func [params out] #:transparent)

#;(define-data ForallType
  [: (forall [Type -> Type])]
  [Interpretation:
   "A parameterized type, created by calling @get-type with any type."]
  [Examples:
   "Takes an instance of any type, returns an instance of the same type." <=
   (forall (λ (X) (func (list X) X)))])
(struct forall [get-type] #:transparent)

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

#;(define-docs (Forall/parsed params out)
    [Signature: [Type -> Type] -> Type]
    [Semantics: #<<"
Creates a parameterized type.
The function @f computes the type given the type parameter.
"
                ]
    [Examples:
     (Forall/parsed (λ (X) X)) => (λ () (forall (λ (X) X)))])
(define (Forall/parsed f)
  (λ () (forall f)))

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
           (func? x)
           (forall? x))))

#;(define-docs (try-func-out x)
    [Signature: Type -> [Maybe Type]]
    [Purpose: #<<"
@f's output if it's function type, otherwise #false.
Assumes parameterized types are given parameters -
returns #false for forall types.
"
              ]
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

#;(define-docs (unparameterize f xs)
    [Signature: Type [Listof Type] -> Type]
    [Purpose: #<<"
Uses @xs to find possible parameters for each parameterized type in @f.
Applies each parameterized type with each of these parameters,
and replaces it with the intersection of the results.
"
              ])
(define (unparameterize f xs)
  (define (unparameterize* f*)
    (unparameterize f* xs))
  (define f+ (f))
  (cond
    [(primitive? f+) f]
    [(intersection? f+)
     (λ () (intersection (map unparameterize* (intersection-subs f+))))]
    [(union? f+)
     (λ () (union (map unparameterize* (union-subs f+))))]
    [(func? f+)
     (λ () (func (map unparameterize* (func-params f+))
                 (unparameterize* (func-out f+))))]
    [(forall? f+) (unparameterize* (app-forall f+ xs))]
    [(list? f+) (λ () (map unparameterize* f+))]
    [else f]))

#;(define-docs (app-forall f xs)
    [Signature: ForallType [Listof Type] -> Type]
    [Purpose: #<<"
Uses @xs to find possible parameters for the parameterized type @f.
Applies @f with each of these parameters,
and returns the intersection of the results.
"
              ])
(define (app-forall f xs)
  (λ () (intersection (map (forall-get-type f) (find-parameters xs)))))

#;(define-docs (find-parameters xs)
    [Signature: [Listof Type] -> [Listof Type]]
    [Purpose: #<<"
Finds parameters which can be applied to a parameterized type
to yield types which can then be related with @xs.
"
              ])
(define (find-parameters xs)
  (list* (λ () (union '())) ; Nothing
         (λ () (intersection '())) ; Any
         (λ () (union xs))
         (λ () (intersection xs))
         xs))

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
       (type-summary/recur type
                           (- summary-recur-limit recurs-left)
                           (λ (_) #true)
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
      [else (type-summary/recur type
                                (- summary-recur-limit recurs-left)
                                (λ (x) (not (expr? (x))))
                                (curryr type-label/acc (sub1 recurs-left)))]))
  (type-label/acc type summary-recur-limit))

#;(define-docs (type-summary/recur type eid sub-filter sub-summary)
    [Signature: Type Nat [Type -> Bool] [Type -> String] -> String]
    [Purpose: #<<"
A summary of the type. Fills in parameterized types with the primitive "X@eid",
ignores intersection sub-types which return false in @sub-filter
(pretends that they don't exist), and summarizes sub-types via @sub-summary.
"
              ]
    [Examples:
     (type-summary/recur (λ () (primitive "String")) (λ (x) "foo")) => "String"
     (type-summary/recur Nat (λ (x) "foo")) => "[Intersection foo foo]"])
(define (type-summary/recur type eid sub-filter sub-summary)
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
    [(forall? type+)
     (define eparam (λ () (primitive (format "X~a" eid))))
     (format "{~a} ~a"
             (sub-summary eparam)
             (sub-summary ((forall-get-type type+) eparam)))]
    [else (format "~v" type+)]))