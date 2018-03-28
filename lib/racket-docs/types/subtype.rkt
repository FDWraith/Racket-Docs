#lang racket

(provide type-comparison-limit
         nothing/unknown?
         type=?
         type<?
         types<?
         params<?
         refine-for-params)

(require "struct.rkt")

#;(define-docs type-comparison-limit
    [Signature: Nat]
    [Purpose: #<<"
After this many nested comparisons, assumes 1 type is a subtype of another.
"
              ])
(define type-comparison-limit 32)

#;(define-docs (nothing/unknown? x)
    [Signature: Type -> Bool]
    [Purpose: "Whether the type is nothing or unknown."]
    [Examples:
     (nothing/unknown? String) => #false
     (nothing/unknown? Nothing) => #true])
(define (nothing/unknown? x)
  (define un-x (x))
  (and (intersection? un-x) (empty? (intersection-subs un-x))))

#;(define-docs (type=? x y)
    [Signature: Type Type -> Bool]
    [Purpose: #<<"
Whether @x and @y are equal - whether @x and @y are subtypes of each other.
"
              ]
    [Examples:
     (type=? Nat Nat) => #true
     (type=? Int Num) => #false
     (type=? Num Int) => #false
     (type=? [Union Int String] [Union Int String]) => #true])
(define (type=? x y)
  (and (type<? x y) (type<? y x)))

#;(define-docs (type<? x y)
    [Signature: Type Type -> Bool]
    [Purpose: #<<"
Whether @x is a subtype of @y -
whether any instance of @x is also an instance of @y.
Note that an unknown type is a subtype of any type.
"
              ]
    [Examples:
     (type<? Int String) => #false
     (type<? Int Num) => #true
     (type<? Unknown Int) => #true])
(define (type<? x y)
  (type<?/limit x y type-comparison-limit))

#;(define-docs (type<?/limit x y limit)
    [Signature: Type Type Nat -> Bool]
    [Purpose: #<<"
Whether @x is a subtype of @y -
whether any instance of @x is also an instance of @y.
Note that an unknown type is a subtype of any type.
Gives up and returns true after @limit nested comparisons.
"
              ]
    [Examples:
     (type<?/limit Int String 2) => #false
     (type<?/limit Int Num 3) => #true
     (type<?/limit Unknown Int 1) => #true
     (type<?/limit Int String 0) => #true])
(define (type<?/limit x y limit)
  (un-type<?/limit (x) (y) limit))

#;(define-docs (un-type<?/limit x y limit)
    [Signature: UnwrappedType UnwrappedType -> Bool]
    [Purpose: #<<"
Whether @x is a subtype of @y -
whether any instance of @x is also an instance of @y.
Note that an unknown type is a subtype of any type.
Gives up and returns true after @limit nested comparisons.
"
              ]
    [Examples:
     (un-type<?/limit (Int) (String) 2) => #false
     (un-type<?/limit (Int) (Num) 3) => #true
     (un-type<?/limit (Unknown) (Int) 1) => #true
     (un-type<?/limit (Int) (String) 0) => #true])
(define (un-type<?/limit x y limit)
  (define (un-type>x? y*)
    (un-type<?/limit x (y*) (sub1 limit)))
  (define (un-type<y? x*)
    (un-type<?/limit (x*) y (sub1 limit)))
  (cond
    [(zero? limit) #true]
    [(union? x) (andmap un-type<y? (union-subs x))]
    [(intersection? y) (andmap un-type>x? (intersection-subs y))]
    [(intersection? x) (ormap un-type<y? (intersection-subs x))]
    [(union? y) (ormap un-type>x? (union-subs y))]
    [(and (primitive? x) (primitive? y)) (equal? x y)]
    [(and (func? x) (func? y))
     (and (types<?/limit (func-params y) (func-params x) (sub1 limit))
          (type<?/limit (func-out x) (func-out y) (sub1 limit)))]
    [(and (list? x) (list? y)) (types<?/limit x y (sub1 limit))]
    [else (equal? x y)]))

#;(define-docs (types<? xs ys)
    [Signature: [Listof Type] [Listof Type] -> Bool]
    [Purpose: #<<"
Whether there are equal amounts of @xs and @ys,
and each element in @xs is a subtype of the element in @ys with the same index.
"
              ])
(define (types<? xs ys)
  (types<?/limit xs ys type-comparison-limit))

#;(define-docs (types<?/limit xs ys limit)
    [Signature: [Listof Type] [Listof Type] -> Bool]
    [Purpose: #<<"
Whether there are equal amounts of @xs and @ys,
and each element in @xs is a subtype of the element in @ys with the same index.
Gives up and returns true after @limit nested comparisons.
"
              ])
(define (types<?/limit xs ys limit)
  (and (equal? (length xs) (length ys))
       (andmap (curryr type<?/limit limit) xs ys)))

#;(define-docs (params<? xs f)
    [Signature: [Listof Type] Type -> Bool]
    [Purpose: #<<"
Whether @xs are subtypes of the parameters of @f.
If @f is a union type, whether @xs are subtypes of all of @f's subtypes.
If @f is an intersection type, whether @xs are subtypes of any of @f's subtypes.
If @f is a function, whether @xs are subtypes of the parameters.
Otherwise, #false.
"
              ])
(define (params<? xs f)
  (define f+ (f))
  (cond
    [(intersection? f+)
     (ormap (curry params<? xs) (intersection-subs f+))]
    [(union? f+)
     (andmap (curry params<? xs) (union-subs f+))]
    [(func? f+) (types<? xs (func-params f+))]
    [else #false]))

#;(define-docs (refine-for-params xs f)
    [Signature: [Listof Type] Type -> [Maybe Type]]
    [Purpose: #<<"
Treating @f as an intersection or union of functions,
returns only the functions which satisfy the given parameters.
"
              ])
(define (refine-for-params xs f)
  (define f+ (f))
  (cond
    [(intersection? f+)
     (λ () (intersection (filter-map (curry refine-for-params xs)
                                     (intersection-subs f+))))]
    [(union? f+)
     (and (andmap (curry params<? xs) (union-subs f+))
          f)]
    [(func? f+)
     (and (types<? xs (func-params f+))
          f)]
    [else #false]))