#lang racket

;; This file contains functions for type checking.

(require syntax/parse
         syntax/stx
         racket/syntax
         "stx-utils.rkt")

;; Stx is a syntax object, as defined by Racket
;; An Id is Stx that is an identifier
;; - see http://docs.racket-lang.org/guide/stx-obj.html

;; A Term is Stx that has 'type stx prop that is a Type
;; - NOTE:
;; A Term must be expanded Stx since the 'type prop
;; is only attached after expansion.
;; We continue to use Stx as the data def of unexpanded Terms, e.g.
;; see assign-type or type-of.

;; A Datum is an S-expr

(provide ; i.e., my wish list

 ; assign-type : Stx Type -> Term
 ; Creates a Term from a Stx by attaching the given Type.
 assign-type

 ; type-of : Stx -> [List Term Type]
 ; Computes a Type and Term from the given Stx.
 type-of

 ; type-of/ctx : Stx TyEnv -> [List Ids Term Type]
 ; Like `type-of`, but accepts an extra type environment context.
 ; Computes a Type and Term from the given Stx and TyEnv.
 ; Also returns a list of binders for var refs in the returned Term.
 type-of/ctx

 ; type=? : Type Type -> Bool
 ; Returns true if two types are equal.
 type=?

 ; tyerr-msg : Type Type -> String
 ; prints type err msg with given "expected" and "actual" types
 tyerr-msg

 ;; type of types ----------------------------------------

 ; mk-type : Stx -> Type
 ; Creates a Type from a Stx
 mk-type
 ; type? : Stx -> Bool
 ; Returns true if the argument is a Type.
 type?
 ; types? : Stx -> Bool
 ; Returns true if input is stxobj whose elemenets are all Types.
 types?
 ; syntax class forms of the above predicates
 type
 types
 )


(define TYPE-TAG 'type) ; key for type stx prop


;; assign-type : Stx Type -> Term
;; Creates a Term from a Stx by attaching the given Type.
(define (assign-type e t)
  (syntax-property e TYPE-TAG t))

;; get-type : Term -> Type
;; Returns the TYPE-TAG property from the given Term.
(define (get-type e)
  (ca*r (syntax-property e TYPE-TAG)))

 ; type-of : Stx -> [List Term Type]
 ; Computes a Type and Term from a given Stx.
(define (type-of e)
  (define e+ (local-expand e 'expression null))
  (list e+ (get-type e+)))

; type=? : Type Type -> Boolean
; Returns true if the two given types are equivalent.
(define (type=? t1 t2)
      ;; type may be id
  (or (and (identifier? t1) (identifier? t2)
           (free-identifier=? t1 t2))
      ;; or compound type (ie, type constructor)
      (and (stx-pair? t1) (stx-pair? t2)
           (all type=? t1 t2))))

; type->datum : Type -> s-expr
; Converts a Type into a more printable representation.
(define (type->datum t)
  (syntax->datum t))

;; Type Type -> String
;; Returns an appropriate type error message when the given
;; `expected` and `actual` types differ.
(define (tyerr-msg expected actual)
  (format "type mismatch, expected ~a, got ~a"
          (type->datum expected) (type->datum actual)))


;; ----------------------------------------------------------------------------
;; types for types

;; #%type = the type of types, i.e., a "kind"
(define (#%type) (error '#%type "can't use at runtime"))

(define (mk-type t)
  (assign-type t #'#%type))

(define (type? t)
  (define tt (type-of t))
  (and tt (type=? (second tt) #'#%type)))
(define (types? ts)
  (all type? ts))

(define-syntax-class type
  (pattern (~and τ (~fail #:unless (type? #'τ) "expected type"))))
(define-syntax-class types
  (pattern (τ:type ...)
           #:attr len (length (syntax->list #'(τ ...)))))

;; ----------------------------------------------------------------------------
;; expanding with contexts

;; A TyEnv is Stx with shape ([Id Type] ...)
;; An ExpandCtx is a definition context, as recognized by local-expand

; type-of/ctx : Stx TyEnv -> [List Ids Term Type]
(define (type-of/ctx e Γ)
  (match-define (list xs+ CTX) (tyenv->expandctx Γ))
  (define e+ (local-expand e 'expression null CTX))
  (list xs+ e+ (get-type e+)))

;; TyEnv -> [List Ids ExpandCtx]
;; intuition:
;;   for each x : τ in TyEnv:
;;     define x as macro that expands into (assign-type #'y #'τ)
;;     where y is a new, untyped var
;; Returns the ExpandCtx and the new y's
(define (tyenv->expandctx Γ)
  (syntax-parse Γ
    [([x τ] ...)
     (define ctx (syntax-local-make-definition-context))
     (define (fresh/ctx s)
       (internal-definition-context-introduce
        ctx
        (generate-temporary s)))
     (define xs+ (stx-map fresh/ctx #'(x ...)))
     (syntax-local-bind-syntaxes xs+ #f ctx)
     (syntax-local-bind-syntaxes
      (syntax->list #'(x ...))
      (with-syntax ([(x+ ...) xs+])
        #`(values
           (mk-applied-id-macro (assign-type #'x+ #'τ)) ...))
      ctx)
     (list xs+ ctx)]))
