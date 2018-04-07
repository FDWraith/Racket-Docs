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
         Nothing/parsed
         Any/parsed
         expr?
         expr-func?
         try-func-params
         try-func-out
         unparameterize
         unparameterize/found
         app-forall
         app-forall/found
         find-parameters
         fill-forall)

(require "../utils/gen.rkt")

#;(define-data Type
    [: - (labeled-type [-> UnwrappedType] String Boolean)
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

#;(define-docs subtype-count-limit
    [Signature: Nat]
    [Purpose: "How many parts of a type will be tried as parameters."])
(define subtype-count-limit 3)

#;(define-docs (Intersection/parsed xs)
    [Signature: Type -> Type]
    [Purpose: #<<"
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
    [Purpose: #<<"
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
    [Purpose: #<<"
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
    [Purpose: #<<"
Creates a parameterized type.
The function @f computes the type given the type parameter.
"
              ]
    [Examples:
     (Forall/parsed (λ (X) X)) => (λ () (forall (λ (X) X)))])
(define (Forall/parsed f)
  (λ () (forall f)))

#;(define-docs Nothing/parsed
    [Signature: Type]
    [Purpose: "This is a subtype of all other types."])
(define Nothing/parsed (Union/parsed '()))

#;(define-docs Any/Parsed
    [Signature: Type]
    [Purpose: "All other types are a subtype of this."])
(define Any/parsed (Intersection/parsed '()))

#;(define-docs (expr? x)
    [Signature: Type -> Bool]
    [Purpose: "Whether x is an expression type."]
    [Examples:
     (expr? String) => #false
     (expr? 'a) => #true
     (expr? `(a ,String)) => #true])
(define (expr? x)
  (not (or (primitive? x)
           (intersection? x)
           (union? x)
           (func? x)
           (forall? x))))

#;(define-docs (expr-func? x)
    [Signature: Type -> Bool]
    [Purpose: #<<"
Whether x is an expression type of a function being applied to other types.
"
              ]
    [Examples:
     (expr-func? String) => #false
     (expr-func? 'a) => #false
     (expr-func? `(a ,String)) => #true
     (expr-func? ''a) => #false]) ; This is tricky because ''a is a list.
(define (expr-func? x)
  (and (cons? x)
       (not (equal? (car x) 'quote))))

#;(define-docs (try-func-params x)
    [Signature: Type -> [Maybe [Listof Type]]]
    [Purpose: #<<"
@f's parameters if it includes a function type, otherwise #false.
Assumes parameters are as generic as possible -
assumes forall type variables are all Nothing,
intersection/union types yield intersection/union parameters,
and the # of parameters is the # of parameters in the smallest function.
"
              ]
    [Examples:
     (try-func-params (->/parsed (list Int Nat) String)) => (list Int Nat)
     (try-func-params
      (Intersection/parsed (list (->/parsed (list Int) String)
                                 (->/parsed (list String) Bool)))) =>
     (Intersection/parsed (list Int String))
     (try-func-params String) => Any])
(define (try-func-params x)
  (define (try-sub-params mk-sub-unwrapped xs)
    (map mk-sub-unwrapped (transpose (filter-map try-func-params xs))))
  (define x+ (x))
  (cond
    [(func? x+) (func-params x+)]
    [(intersection? x+)
     (try-sub-params intersection (intersection-subs x+))]
    [(union? x+)
     (try-sub-params union (union-subs x+))]
    [(forall? x+)
     (try-func-params ((forall-get-type x+) Nothing/parsed))]
    [else #false]))

#;(define-docs (try-func-out x)
    [Signature: Type -> [Maybe Type]]
    [Purpose: #<<"
@f's output if it's function type, otherwise #false.
Applies Nothing and Any to forall types.
"
              ]
    [Examples:
     (try-func-out (->/parsed (list Int Nat) String)) => String
     (try-func-out String) => Any])
(define (try-func-out x)
  (define x+ (x))
  (cond
    [(func? x+) (func-out x+)]
    [(intersection? x+)
     (define sub-outs (filter-map try-func-out (intersection-subs x+)))
     (and (cons? sub-outs)
          (λ () (intersection sub-outs)))]
    [(union? x+)
     (define sub-outs (map try-func-out (union-subs x+)))
     (and (cons? sub-outs)
          (not (member #false sub-outs))
          (λ () (union sub-outs)))]
    [(forall? x+) (try-func-out ((forall-get-type x+) Any/parsed))]
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
  (unparameterize/found f (find-parameters xs)))

#;(define-docs (unparameterize/found f xs)
    [Signature: Type [Listof Type] -> Type]
    [Purpose: #<<"
Applies each parameterized type with each of the given parameters,
and replaces it with the intersection of the results.
"
              ])
(define (unparameterize/found f xs)
  (define (unparameterize/found* f*)
    (unparameterize/found f* xs))
  (define f+ (f))
  (cond
    [(primitive? f+) f]
    [(intersection? f+)
     (λ () (intersection (map unparameterize/found* (intersection-subs f+))))]
    [(union? f+)
     (λ () (union (map unparameterize/found* (union-subs f+))))]
    [(func? f+)
     (λ () (func (map unparameterize/found* (func-params f+))
                 (unparameterize/found* (func-out f+))))]
    [(forall? f+) (unparameterize/found* (app-forall/found f+ xs))]
    [(expr-func? f+) (λ () (map unparameterize/found* f+))]
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
  (app-forall/found f (find-parameters xs)))

#;(define-docs (app-forall/found f xs)
    [Signature: ForallType [Listof Type] -> Type]
    [Purpose: #<<"
Applies @f with each of the given parameters,
and returns the intersection of the results.
"
              ])
(define (app-forall/found f xs)
  (λ () (intersection (map (forall-get-type f) xs))))

#;(define-docs (find-parameters xs)
    [Signature: [Listof Type] -> [Listof Type]]
    [Purpose: #<<"
Finds parameters which can be applied to a parameterized type
to yield types which can then be related with @xs.
"
              ])
(define (find-parameters xs)
  (define all-xs (append-map find-parts xs))
  (list* Nothing/parsed
         Any/parsed
         all-xs))

#;(define-docs (find-parts x)
    [Signature: Type -> [Listof Type]]
    [Purpose: #<<"
Finds types in @x if @x is a function or expression function.
These could be used as parameters to make a forall type match @x.
"
              ])
(define (find-parts x)
  (find-proper-parts/acc x 0))

(define (find-parts/acc x count)
  (cons x (find-proper-parts/acc x count)))

(define (find-proper-parts/acc x count)
  (define (find-proper-parts* x*)
    (find-proper-parts/acc x* (add1 count)))
  (define (part-find-parts part)
    (find-parts/acc part (add1 count)))
  (define x+ (x))
  (cond
    [(>= count subtype-count-limit) '()]
    [(intersection? x+)
     (append-map find-proper-parts* (intersection-subs x+))]
    [(union? x+)
     (append-map find-proper-parts* (union-subs x+))]
    [(func? x+)
     (append (append-map part-find-parts (func-params x+))
             (part-find-parts (func-out x+)))]
    [(forall? x+)
     (find-proper-parts* ((forall-get-type x+) Nothing/parsed))]
    [(expr-func? x+) (append-map part-find-parts (rest x+))]
    [else '()]))

#;(define-docs (fill-forall x)
    [Signature: Type -> Type]
    [Purpose:
     "Gets rid of forall types by applying them with primitives X0, X1, etc"])
(define (fill-forall x)
  (fill-forall/acc x 0))

(define (fill-forall/acc x num)
  (define (fill-forall* x*)
    (fill-forall/acc x* (add1 num)))
  (cond
    [(labeled-type? x)
     (labeled-type (fill-forall/acc (labeled-type-proc x) num)
                   (labeled-type-label x))]
    [else
     (define x+ (x))
     (cond
       [(primitive? x+) x]
       [(intersection? x+)
        (λ () (intersection (map fill-forall* (intersection-subs x+))))]
       [(union? x+)
        (λ () (union (map fill-forall* (union-subs x+))))]
       [(func? x+)
        (λ () (func (map fill-forall* (func-params x+))
                    (fill-forall (func-out x+))))]
       [(forall? x+)
        (define filler (λ () (primitive (format "X~a" num))))
        (fill-forall* ((forall-get-type x+) filler))]
       [(expr-func? x+) (λ () (map fill-forall* x+))]
       [else x])]))