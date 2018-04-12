#lang racket

(provide gen-define-syntax/docs
         fill-type-args-forall
         fill-type-args-label)

(require "classes.rkt"
         "../utils.rkt"
         syntax/parse
         "../types/struct.rkt"
         [for-template "../types/builtin.rkt"
                       "../types/struct.rkt"
                       [except-in racket/base primitive?]])

(define (gen-define-syntax/docs scope-stx)
  ;define-docs in the local scope
  (with-syntax [(define-docs (datum->syntax scope-stx 'define-docs))]
    (syntax-parser
      #:datum-literals (Semantics:)
      [(_ id:id
          [Semantics: semantics:raw-text]
          extra-prop:extra-doc-prop ...
          option:stxparse-option ...
          [((~var case-temp-head (temp-matches #'id)) case-temp-part ...)
           case-body ...+] ...+)
       #:with (opt-part ...) (flatten/stx #'(option ...))
       #`(begin
           (define-docs id
             [Syntax: (id case-temp-part ...) ...]
             [Semantics: semantics]
             extra-prop ...)
           (define-syntax id
             (syntax-parser
               opt-part ...
               [(case-temp-head case-temp-part ...)
                case-body ...] ...)))]
      [(_ (id:id stx:id)
          [Semantics: semantics:raw-text]
          extra-prop:extra-doc-prop ...
          option:stxparse-option ...
          [(case-temp-head case-temp-part ...)
           case-body ...+] ...+)
       #:with (opt-part ...) (flatten/stx #'(option ...))
       #`(begin
           (define-docs id
             [Syntax: (id case-temp-part ...) ...]
             [Semantics: semantics]
             extra-prop ...)
           (define-syntax (id stx)
             (syntax-parse stx
               opt-part ...
               [(case-temp-head case-temp-part ...)
                case-body ...] ...)))])))

#;(define-docs (fill-type-args-forall args type-body-stx)
  [Signature: [Stx [Listof Identifier]] Syntax -> Syntax]
  [Purpose: #<<"
Wraps @type-body-stx around foralls, which assign its type variables.
"
            ]
  [Examples:
   (fill-type-args-forall #'(X Y Z) #'[Listof X]) =>
   #'[Forall X [Forall Y [Forall Z [Listof X]]]]])
(define (fill-type-args-forall args type-body-stx)
  (cond
    [(not args) type-body-stx]
    [else (fill-type-args-forall/list (syntax->list args) type-body-stx)]))

#;(define-docs (fill-type-args-forall/list args type-body-stx)
  [Signature: [Stx [Listof Identifier]] Syntax -> Syntax]
  [Purpose: #<<"
Wraps @type-body-stx around foralls, which assign its type variables.
"
            ]
  [Examples:
   (fill-type-args-forall/list (list #'X #'Y #'Z) #'[Listof X]) =>
   #'[Forall X [Forall Y [Forall Z [Listof X]]]]])
(define (fill-type-args-forall/list args type-body-stx)
  (cond
    [(empty? args) type-body-stx]
    [else
     (with-syntax [(arg (first args))
                   (type-body (fill-type-args-forall/list (rest args)
                                                          type-body-stx))]
       #'[Forall arg type-body])]))

#;(define-docs (fill-type-args-label args type-body-stx)
  [Signature: [Stx [Listof Identifier]] Syntax -> Syntax]
  [Purpose: #<<"
Wraps @type-body-stx around a let,
which defines its type variables to types with the variables' labels.
"
            ]
  [Examples:
   (fill-type-args-label #'(X Y Z) #'[Listof X]) =>
   #'(let [(X (λ () (primitive "X")))
           (Y (λ () (primitive "Y")))
           (Z (λ () (primitive "Z")))]
       [Listof X])])
(define (fill-type-args-label args type-body-stx)
  (cond
    [(not args) type-body-stx]
    [else
     (with-syntax [((arg ...) args)
                   ((arg-name ...) (map/stx identifier->stx-string args))
                   (type-body type-body-stx)]
       #'(let [(arg (λ () (primitive arg-name))) ...]
           type-body))]))