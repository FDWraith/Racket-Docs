#lang racket

(provide [for-syntax ; Special types
                     Intersection
                     Union
                     ->
                     
                     ; Primitive Types
                     Void
                     Bool
                     PosInt
                     NegInt
                     Decimal
                     Char
                     String

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

(require [for-syntax "struct.rkt"
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
(define-type/parsed+un (Intersection . xs)
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
(define-type/parsed+un (Union . xs)
  (union xs))

#;(define-docs [-> X . Xs]
    [Signature: [Type ... -> Type]]
    [Semantics: #<<"
Creates a function type, with all but the last parameter as
param types, and the last parameter as the output type.
"
                ]
    [Examples: [-> String Bool] => (λ () (func (list String) Bool))])
(define-type/parsed+un (-> arg . rest-args)
  (define args (cons arg rest-args))
  (match-define-values (params (list out)) (split-at-right args 1))
  (func params out))

(define-type/primitive Void)
(define-type/primitive Bool)
(define-type/primitive PosInt)
(define-type/primitive NegInt)
(define-type/primitive Decimal)
(define-type/primitive Char)
(define-type/primitive String)

(define-type Any [Intersection])
(define-type Nothing [Union])
(define-type Unknown Nothing)
(define-type Nat [Union 0 PosInt])
(define-type Int [Union Nat NegInt])
(define-type Num [Union Int Decimal])
(define-type [Listof X]
  [Union '() (cons X [Listof X])])
(define-type [Maybe X]
  [Union #false X])
(define-type List [Listof Any])