#lang racket

(provide gen-define-syntax/docs)

(require "classes.rkt"
         "../utils.rkt"
         syntax/parse)

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