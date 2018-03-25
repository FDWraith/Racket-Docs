#lang racket

(provide nothing/unknown?
         type<?
         types<?
         params<?)

(require "struct.rkt")

#;(define-docs (nothing/unknown? x)
    [Signature: Type -> Bool]
    [Purpose: "Whether the type is nothing or unknown."]
    [Examples:
     (nothing/unknown? String) => #false
     (nothing/unknown? Nothing) => #true])
(define (nothing/unknown? x)
  (define un-x (x))
  (and (intersection? un-x) (empty? (intersection-subs un-x))))

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
  (un-type<? (x) (y)))

#;(define-docs (un-type<? x y)
    [Signature: UnwrappedType UnwrappedType -> Bool]
    [Purpose: #<<"
Whether @x is a subtype of @y -
whether any instance of @x is also an instance of @y.
Note that an unknown type is a subtype of any type.
"
              ]
    [Examples:
     (un-type<? (Int) (String)) => #false
     (un-type<? (Int) (Num)) => #true
     (un-type<? (Unknown) (Int)) => #true])
(define (un-type<? x y)
  (define (un-type>x? y*)
    (un-type<? x (y*)))
  (define (un-type<y? x*)
    (un-type<? (x*) y))
  (cond
    [(union? x) (andmap un-type<y? (union-subs x))]
    [(intersection? y) (andmap un-type>x? (intersection-subs y))]
    [(intersection? x) (ormap un-type<y? (intersection-subs x))]
    [(union? y) (ormap un-type>x? (union-subs y))]
    [(and (primitive? x) (primitive? y)) (equal? x y)]
    [(and (func? x) (func? y))
     (and (types<? (func-params y) (func-params x))
          (type<? (func-out x) (func-out y)))]
    [(and (list? x) (list? y)) (types<? x y)]
    [else (equal? x y)]))

#;(define-docs (types<? xs ys)
    [Signature: [Listof Type] [Listof Type] -> Bool]
    [Purpose: #<<"
Whether there are equal amounts of @xs and @ys,
and each element in @xs is a subtype of the element in @ys with the same index.
"
              ])
(define (types<? xs ys)
  (and (equal? (length xs) (length ys))
       (andmap type<? xs ys)))

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