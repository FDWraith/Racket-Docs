#lang racket
(require (for-syntax (for-syntax racket/base
                                 syntax/parse
                                 "stx-utils.rkt")
                     syntax/parse
                     syntax/stx
                     "typecheck.rkt"
                     "stx-utils.rkt"))

; This function defines forms for defining types and typed langa.

(provide
 ; toplevel forms --------------------

 ; (define-type name)
 ; defines
 ; - (at phase n) a base Type with the given `name`
 ; - (phase n+1) a predicate that recognizes the type
 ; - (phase n+1) a pattern expander that recognizes the type
 define-type

 ; (define-type-constructor name #:arity op n)
 ; Like define-type, except defines a type constructor that when applied,
 ; creates a compound type from other types.
 ; Its arity m must satisfy (op m n).
 define-type-constructor

 ; (define-typed-prim name untypedname : type)
 ; Defines the specified typed prim
 ; by assigning the given type to the given untyped fn.
 define-typed-prim
)

(define-syntax define-type
  (syntax-parser
    [(_ name:id)
     #:with name? (mk-? #'name)
     #:with patmacro (mk-~ #'name)
     #:with internal (fresh #'name)
     #'(begin
         ; The type being defined, to be used by the programmer.
         (define-syntax name
           (syntax-parser
             [:id (mk-type #'internal)]))
         ; Internal representation of the type.
         (define (internal)
           (error 'name "can't use types at runtime"))
         (begin-for-syntax
           ; Predicate recognizing the type.
           (define (name? t) (type=? t #'name))
           ; stx-parse pattern that recognizes the type.
           (define-syntax patmacro
             (pattern-expander
              (mk-id-macro
               #'(~and ty
                       (~fail #:unless (name? #'ty)
                              (tyerr-msg #'name #'ty))))))))]))

(define-syntax define-type-constructor
  (syntax-parser
    [(_ name:id #:arity op:id n:exact-nonnegative-integer)
     #:with name? (mk-? #'name)
     #:with patmacro (mk-~ #'name)
     #:with internal (fresh #'name)
     #'(begin
         ; The type constructor being defined, to be used by the programmer.
         (define-syntax name
           (syntax-parser
             [(_ . (~and
                    args:types
                    (~fail #:unless (op (attribute args.len) n)
                      (format "wrong number of args, expected ~a ~a" 'op 'n))))
              (mk-type #'(internal . args))]))
         ; Internal representation of the type.
         (define (internal . _)
           (error 'name "can't use types at runtime"))
         (begin-for-syntax
           ; Predicate recognizing a type created from the type constructor.
           (define (name? t) 
             (and (stx-pair? t) (type=? (stx-car t) #'name)))
           ; stx-parse pattern recognizing a type from the ty constructor.
           (define-syntax patmacro
             (pattern-expander
              (syntax-parser
                [(_ . pat)
                 #'(~and ty
                         (~fail #:unless (name? #'ty)
                                (tyerr-msg #'name #'ty))
                         (~parse pat (stx-cdr #'ty)))])))))]))

(define-syntax define-typed-prim
  (syntax-parser
    [(_ f unf (~datum :) ty)
     #'(define-syntax f
         (mk-applied-id-macro
          (assign-type #'unf #'ty)))]))

