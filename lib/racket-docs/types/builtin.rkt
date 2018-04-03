#lang racket

(provide [for-syntax ; Special types
                     Intersection
                     Union
                     ->
                     Forall
                     
                     ; Primitive Types
                     Void
                     Bool
                     PosInt
                     NegInt
                     Decimal
                     Char
                     String
                     <Error!>

                     ; Built-In Non-Primitive Types
                     Any
                     Nothing
                     Unknown
                     Nat
                     Int
                     Num
                     Listof
                     Maybe
                     List])

(require [for-syntax [for-syntax racket/base]
                     "struct.rkt"
                     racket/match
                     racket/list]
         "create.rkt")

#;(define-docs [Intersection . Xs]
    [Signature: [Type ... -> Type]]
    [Semantics: #<<"
Creates an intersection type, with the given parameters as sub-types.
A value belongs to this type if it belongs to all of the sub-types.
"
                ]
    [Examples:
     [Intersection String Bool] => (λ () (intersection (list String Bool)))])
(define-type/parsed+un (Intersection . xs) #:no-label
  (intersection xs))

#;(define-docs [Union . Xs]
    [Signature: [Type ... -> Type]]
    [Semantics: #<<"
Creates a union type, with the given parameters as sub-types.
A value belongs to this type if it belongs to any of the sub-types.
"
                ]
    [Examples:
     [Union String Bool] => (λ () (union (list String Bool)))])
(define-type/parsed+un (Union . xs) #:no-label
  (union xs))

#;(define-docs [-> X . Xs]
    [Signature: [Type ... -> Type]]
    [Semantics: #<<"
Creates a function type, with all but the last parameter as
param types, and the last parameter as the output type.
"
                ]
    [Examples: [-> String Bool] => (λ () (func (list String) Bool))])
(define-type/parsed+un (-> arg . rest-args) #:no-label
  (define args (cons arg rest-args))
  (match-define-values (params (list out)) (split-at-right args 1))
  (func params out))

#;(define-docs Forall
    [Syntax: [Forall X F]]
    [Semantics: #<<"
Creates a parameterized type, which is computed by replacing all occurrences
of the type parameter @X in @F.
"
                ]
    [Examples:
     [Forall X [-> X String X]] =>
     (λ () (forall (λ (X) (λ () (func (list X String) X)))))])
(define-type/syntax (Forall x f) #:no-label
  #'(forall (λ (x) f)))

(define-type/primitive Void)
(define-type/primitive Bool)
(define-type/primitive PosInt)
(define-type/primitive NegInt)
(define-type/primitive Decimal)
(define-type/primitive Char)
(define-type/primitive String)
(define-type/primitive <Error!>)

(define-type Any [Intersection])
(define-type Nothing [Union])
(define-type Unknown Nothing)
(define-type Nat [Union 0 PosInt])
(define-type Int [Union Nat NegInt])
(define-type Num [Union Int Decimal])
(define-type [Listof X]
  [Intersection '() (cons X [Listof X])]) ; Technically a Union, but weak typing
(define-type [Maybe X]
  [Intersection #false X]) ; Technically a Union, but weak typing
(define-type List [Listof Any])