#lang racket

(provide define-docs)

(require [for-syntax "classes.rkt"
                     "../struct/doc-prop.rkt"
                     "../utils/syntax.rkt"
                     syntax/parse])

(define (shared-type-err shared-type)
  (format "Property ~a of the same type in a documentation block" shared-type))

(define-syntax define-docs
  (syntax-parser
    #:datum-literals (Signature: Purpose:)
    [(_ head:head
        [Signature: sig:type]
        [Purpose: purpose:raw-text]
        extra:extra-doc ...)
     #:with type-prop (type-doc-prop #'sig)
     #:with purpose-prop (purpose-doc-prop #'purpose)
     #:with extra-props (attributes (extra.prop))
     #:with shared-types* (shared-types extra-pr
     #:fail-when (shares-type? (attributes (extra.out ...))) shared-type-err
     ...]))

(define-sytax define-data
  (syntax-parser
    #:datum-literals (: Interpretation:)
    [(_ id:id
        [: sig:union-type]
        [Interpretation: interpretation:raw-text]
        extra:extra-doc ...)
     #:fail-when (shares-type? extra) shared-type-err
     ...]))