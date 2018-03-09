#lang racket

(provide define-parse-class
         define-splicing-parse-class
         parse-class
         parse-classes)

(require [for-syntax "syntax.rkt"
                     syntax/parse
                     racket/syntax]
         syntax/parse)

; Parse class - A syntax class which both matches an expression and parses it.
; Functions as both define-syntax-class (match) and define-syntax (parse).

(begin-for-syntax
  (define (define-gen-parse-class define-gen-syntax-class-stx)
    (syntax-parser
      [(_ id:id
          stxclass-option:stxparse-option ...
          [in-match pattern-option:pattern-option ... out-expr ...+] ...+)
       #:with (stxclass-opt-part ...) (flatten/stx #'(stxclass-option ...))
       #:with ((pattern-opt-part ...) ...)
       (map/stx flatten/stx #'((pattern-option ...) ...))
       #`(#,define-gen-syntax-class-stx
          id
          stxclass-opt-part ...
          [pattern in-match
                   pattern-opt-part ...
                   #:attr out (begin out-expr ...)] ...)])))

(define-syntax define-parse-class
  (define-gen-parse-class #'define-syntax-class))

(define-syntax define-splicing-parse-class
  (define-gen-parse-class #'define-splicing-syntax-class))

(define-syntax parse-class
  (syntax-parser
    [(_ id:id)
     #:with id.out (format-id #'id "~a.out" (syntax-e #'id))
     #'(attribute id.out)]))

(define-syntax parse-classes
  (syntax-parser
    [(_ (id:id (~literal ...)))
     #:with id.out (format-id #'id "~a.out" (syntax-e #'id))
     #'(attribute id.out)]))