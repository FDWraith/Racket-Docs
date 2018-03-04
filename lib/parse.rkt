#lang turnstile

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
     (add-doc! #'head.id
               (list* (sig-doc-prop #'sig)
                      (purpose-doc-prop (parse-class purpose))
                      (parse-classes (extra-prop ...)))
               'define-docs stx #'(extra-prop ...))
     #'(void)]))

(define-syntax (define-data stx)
  (syntax-parse stx
    #:datum-literals (: Interpretation:)
    [(_ id:id
        [: type:union-type]
        [Interpretation: interpretation:raw-text]
        extra-prop:extra-data-doc-prop ...)
     (add-doc! #'id
               (list* (type-doc-prop #'type)
                      (interpretation-doc-prop (parse-class interpretation))
                      (parse-classes (extra-prop ...)))
               'define-data stx #'(extra-prop ...))
     #'(void)])) ; TODO replace (void) with type definition

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
   (define all-docs cur-entries)
   #`'#,(datum->syntax #false all-docs)))