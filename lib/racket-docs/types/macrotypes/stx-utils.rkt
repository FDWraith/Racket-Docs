#lang racket

;; This file contains functions for manipulating stx objects.

(require racket/syntax
         syntax/parse)

(provide (all-defined-out)) ; export everything

;; Stx is a syntax object, as defined by Racket
;; An Id is Stx that is an identifier
;; see http://docs.racket-lang.org/guide/stx-obj.html

;; Id -> Id
;; appends '?' to given id
(define (mk-? id) (format-id id "~a?" id))

;; Id -> Id
;; prefixes ~ to given id
(define (mk-~ id) (format-id id "~~~a" id))

;; equivalent to "stx-andmap", but with a shorter name
;; all : (-> Stx Bool) Stx ... -> Bool
(define (all p? . stxs)
  (apply andmap p? (map syntax->list stxs)))

;; returns an id with the same name as id, but with new scopes
(define (fresh id)
  ((make-syntax-introducer) (datum->syntax #f (syntax-e id))))

;; Stx -> (Stx -> Stx)
;; Returns a stx transformer that replaces its name in the input stxobj,
;; if it's in expression position, with the given out-stx.
(define (mk-id-macro out-stx)
  (syntax-parser
    [:id out-stx]
    [(_ . rst) #`(#,out-stx . rst)]))

;; Stx -> (Stx -> Stx)
;; Returns a stx transformer that replaces its name in the input stxobj,
;; if it's in expression position, with the given out-stx,
;; but uses in operator pos are assumed to be function application.
(define (mk-applied-id-macro out-stx)
  (syntax-parser
    [:id out-stx]
    [nonid ; insert app and recursively call first clause of this macro
     #:with app (datum->syntax #'nonid '#%app #'nonid)
     (datum->syntax #'(app . nonid) (cons #'app #'nonid) #'nonid)]))

; Type -> Type
; Fixes a syntax property type, since sometimes it gets corrupted.
(define (ca*r x)
  x)