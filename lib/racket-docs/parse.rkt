#lang typed/racket

(provide define-docs
         define-data
         get-all-docs)

(require [for-syntax "parse/classes.rkt"
                     "parse/errors.rkt"
                     "struct.rkt"
                     "utils.rkt"
                     syntax/parse])

(define-syntax (define-docs stx)
  (syntax-parse stx
    #:datum-literals (Signature: Purpose:)
    [(_ head:head
        [Signature: sig:type]
        [Purpose: purpose:raw-text]
        extra-prop:extra-doc-prop ...)
     (define sig+ (parse-class sig))
     (add-doc! #'head.id
               (list* (sig-doc-prop sig+)
                      (purpose-doc-prop (parse-class purpose))
                      (parse-classes (extra-prop ...)))
               'define-docs stx #'(extra-prop ...))
     #`(: head.id : #,sig+)]))

(define-syntax (define-data stx)
  (syntax-parse stx
    #:datum-literals (: Interpretation:)
    [(_ id:id
        [: type:union-type]
        [Interpretation: interpretation:raw-text]
        extra-prop:extra-data-doc-prop ...)
     (define type+ (parse-class type))
     (add-doc! #'id
               (list* (type-doc-prop type+)
                      (interpretation-doc-prop (parse-class interpretation))
                      (parse-classes (extra-prop ...)))
               'define-data stx #'(extra-prop ...))
     #`(define-type id #,type+)])) ; FAR-TODO Add support for type constructors

(begin-for-syntax
  (define cur-entries '())
  
  (define (add-doc! id-stx props caller stx shared-stx)
    (define shared-type (check-shared-types props))
    (when shared-type
      (raise-syntax-error caller
                          (shared-type-err shared-type)
                          stx shared-stx))
    (define entry (doc-entry id-stx props))
    (set! cur-entries (cons entry cur-entries))))

(define-syntax get-all-docs
  (mk-id-macro
   ; cur-entries are ordered from bottom of file to top.
   ; User expects entries to be ordered from top of file to bottom.
   (define all-docs (reverse cur-entries))
   #`'#,(datum->syntax #false all-docs)))