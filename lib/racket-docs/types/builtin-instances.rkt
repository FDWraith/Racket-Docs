#lang racket/base

(require [for-syntax syntax/parse
                     racket/base]
         "language.rkt"
         "use.rkt"
         "builtin.rkt"
         [prefix-in un. racket])


(provide +
         -
         *
         /
         first
         rest
         second
         third
         fourth
         fifth
         cons
         append
         string-append
         list)

(define-typed-prim +
  [Intersection [-> Nat Nat Nat]
                [-> Int Int Int]
                [-> PosInt PosInt PosInt]
                [-> NegInt NegInt NegInt]
                [-> Num Num Num]])
(define-typed-prim -
  [Intersection [-> Nat Nat Nat] ; Not always true - weak typing
                [-> Int Int Int]
                ; Not always trye - weak typing
                [-> PosInt PosInt PosInt]
                ; Not always true - weak typing
                [-> NegInt NegInt NegInt]
                [-> Num Num Num]])
(define-typed-prim *
  [Intersection [-> Nat Nat Nat]
                [-> Int Int Int]
                [-> PosInt PosInt PosInt]
                [-> NegInt NegInt NegInt]
                [-> Num Num Num]])
(define-typed-prim /
  [Intersection [-> Nat Nat Nat] ; Not always true - weak typing
                [-> Int Int Int] ; Not always true - weak typing
                [-> Num Num Num]])

(define-typed-prim first
  [Forall X [-> (cons X Any) X]])

(define-typed-prim rest
  [Forall X [-> (cons Any X) X]])

(define-typed-prim second
  [Forall X [-> (cons Any (cons X Any)) X]])

(define-typed-prim third
  [Forall X [-> (cons Any (cons Any (cons X Any))) X]])

(define-typed-prim fourth
  [Forall X [-> (cons Any (cons Any (cons Any (cons X Any)))) X]])

(define-typed-prim fifth
  [Forall X [-> (cons Any (cons Any (cons Any (cons Any (cons X Any))))) X]])

(define-typed-prim cons
  [Forall X [-> X [Listof X] [Listof X]]])

(define-typed-prim append
  [Forall X [-> [Listof X] [Listof X] [Listof X]]])

(define-typed-prim string-append
  [-> String String String])

(define syntax-scope
  (make-syntax-delta-introducer #'cons #false))

(define-syntax list
  (syntax-parser
    [(_) #''()]
    [(_ x xs ...)
     (datum->syntax this-syntax
                    `(,#'cons ,#'x ,#'(list xs ...)))]))