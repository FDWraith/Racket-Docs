#lang racket

(provide define-type
         define-type/primitive
         define-type/parsed+un)

(require [for-syntax "parse.rkt"
                     "struct.rkt"
                     syntax/parse])

#;(define-docs define-type
    [Syntax: (define-type defd:id defn)]
    [Semantics: #<<"
Parses defn - e.g. makes it an expression type if lowercase.
Creates a new type, defd, which is equivalent to defn - whenever an instance of
defn is expected, an instance of defd can be provided, and vice versa.
If defd is a list, it creates a function type which takes the given parameters
(which MUST be valid type identifiers - they can't start with lowercase
characters) and replaces them in defn to get a result.
"
                ]
    [Examples:
     (define-type Integer Int)
     (define-type Int [Union Pos 0 (- Pos)])
     (define-type Listof-String [Union '() (cons String Listof-String)])])
(define-syntax (define-type stx)
  (syntax-parse stx
    [(_ defd:id defn)
     #:with defn+ (parse-type #'defn)
     #'(define-type/parsed+un defd (defn+))]
    [(_ (defd:id param:id ...) defn)
     #:with defn+ (parse-type #'defn)
     (define bad-param-stx
       (findf (compose not type-identifier?) (syntax->list #'(param ...))))
     (when bad-param-stx
       (raise-syntax-error 'define-type
                           "Param must be valid type identifier"
                           stx
                           bad-param-stx))
     #'(define-type/parsed+un (defd param ...) (defn+))]))

#;(define-docs define-type/primitive
    [Syntax: (define-type/primitive defd:id)]
    [Semantics: "Creates a new primitive type and defined defd to it."]
    [Examples: "Defines the type Bool." <= (define-type/parsed Bool)])
(define-syntax define-type/primitive
  (syntax-parser
    [(_ defd:id)
     #:with defd-label (datum->syntax #'defd (symbol->string (syntax-e #'defd)))
     #'(define-type/parsed+un defd (primitive defd-label))]))

#;(define-docs define-type/parsed+un
    [Syntax:
     (define-type/parsed+un defd:id defn)
     (define-type/parsed+un (defd:id param:id ...) defn ...)
     (define-type/parsed+un (defd:id param:id ... . rest:id) defn ...)]
    [Semantics: #<<"
Assumes defn is already parsed - e.g. it can be a primitive type.
Creates a new type, defd, which is equivalent to defn - whenever an instance of
defn is expected, an instance of defd can be provided, and vice versa.
If defd is a list, it creates a function type which takes the given parameters
and creates a result using (defn ...).
"
                ]
    [Examples:
     (define-type Integer (primitive))
     (define-type Int [Union Pos 0 (- Pos)])])
(define-syntax define-type/parsed+un
  (syntax-parser
    [(_ defd:id defn)
     #'(define-for-syntax defd
         (λ () defn))]
    [(_ (defd:id param:id ...) defn ...)
     #'(define-for-syntax (defd param ...)
         (λ () defn ...))]
    [(_ (defd:id param:id ... . rest:id) defn ...)
     #'(define-for-syntax (defd param ... . rest)
         (λ () defn ...))]))