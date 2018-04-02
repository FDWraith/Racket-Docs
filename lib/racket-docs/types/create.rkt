#lang racket

(provide [for-syntax define-type
                     define-type/parsed
                     define-type/parsed+un]
         define-type
         define-type/parsed
         define-type/primitive
         define-type/parsed+un
         define-type/syntax)

(require [for-syntax [for-syntax "parse.rkt"
                                 "desc.rkt"
                                 "struct.rkt"
                                 syntax/parse
                                 racket/string
                                 [except-in racket/base
                                            primitive?]]
                     "desc.rkt"
                     "struct.rkt"
                     racket/string
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
(define-syntax define-type
  (syntax-parser
    [(_ x ...) #'(begin-for-syntax (define-type x ...))]))

(begin-for-syntax
  ; See phase 0 definition for docs
  (define-syntax (define-type stx)
    (syntax-parse stx
      [(_ defd:id defn)
       #:with defn+ (parse-type #'defn)
       #'(define-type/parsed defd defn+)]
      [(_ (defd:id param:id ...) defn)
       #:with defn+ (parse-type #'defn)
       #'(define-type/parsed (defd param ...) defn+)])))

#;(define-docs define-type/parsed
    [Syntax: (define-type/parsed defd:id defn)]
    [Semantics: #<<"
Assumes defn is already parsed, but not unwrapped.
Creates a new type, defd, which is equivalent to defn - whenever an instance of
defn is expected, an instance of defd can be provided, and vice versa.
If defd is a list, it creates a function type which takes the given parameters
(which MUST be valid type identifiers - they can't start with lowercase
characters) and replaces them in defn to get a result.
"
                ]
    [Examples:
     (define-type/parsed Integer Int)
     (define-type/parsed Int [Union Pos 0 (- Pos)])
     (define-type/parsed Empty-List (λ () '()))])
(define-syntax define-type/parsed
  (syntax-parser
    [(_ x ...) #'(begin-for-syntax (define-type/parsed x ...))]))

(begin-for-syntax
  ; See phase 0 definition for docs
  (define-syntax (define-type/parsed stx)
    (syntax-parse stx
      [(_ defd:id defn)
       #'(define-type/parsed+un defd (defn))]
      [(_ (defd:id param:id ...) defn)
       (define bad-param-stx
         (findf (compose not type-identifier?) (syntax->list #'(param ...))))
       (when bad-param-stx
         (raise-syntax-error 'define-type
                             "Param must be valid type identifier"
                             stx
                             bad-param-stx))
       #'(define-type/parsed+un (defd param ...) (defn))])))

#;(define-docs define-type/primitive
    [Syntax: (define-type/primitive defd:id)]
    [Semantics: "Creates a new primitive type and defined defd to it."]
    [Examples: "Defines the type Bool." <= (define-type/parsed Bool)])
(define-syntax define-type/primitive
  (syntax-parser
    [(_ x ...) #'(begin-for-syntax (define-type/primitive x ...))]))

(begin-for-syntax
  ; See phase 0 definition for docs
  (define-syntax define-type/primitive
    (syntax-parser
      [(_ defd:id)
       #:with defd-label
       (datum->syntax #'defd (symbol->string (syntax-e #'defd)))
       #'(define-type/parsed+un defd (primitive defd-label))])))

#;(define-docs define-type/parsed+un
    [Syntax:
     (define-type/parsed+un defd:id defn)
     (define-type/parsed+un (defd:id param:id ...) defn ...)
     (define-type/parsed+un (defd:id param:id ... . rest:id) #:no-label
       defn ...)
     (define-type/parsed+un (defd:id param:id ... . rest:id) defn ...)]
    [Semantics: #<<"
Assumes defn is already parsed and unwrapped - e.g. it can be a primitive type.
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
    [(_ x ...) #'(begin-for-syntax (define-type/parsed+un x ...))]))

(begin-for-syntax
  ; See phase 0 definition for docs
  (define-syntax define-type/parsed+un
    (syntax-parser
      [(_ defd:id defn)
       #:with defd-label
       (datum->syntax #'defd (symbol->string (syntax-e #'defd)))
       #'(define defd
           (labeled-type (λ () defn) defd-label))]
      [(_ (defd:id param:id ...) defn ...)
       #:with defd-head-label
       (datum->syntax #'defd (symbol->string (syntax-e #'defd)))
       #'(define (defd param ...)
           ; Sometimes types in rest aren't parsed, crashing label ...
           (define defd-label
             (if (or (not (procedure? param)) ...)
                 "???"
                 (type-con-label (list defd-head-label
                                       (type-label param) ...))))
           (labeled-type (λ () defn ...) defd-label))]
      [(_ (defd:id param:id ... . rest:id) #:no-label defn ...)
       #:with defd-head-label
       (datum->syntax #'defd (symbol->string (syntax-e #'defd)))
       #'(define (defd param ... . rest)
           (λ () defn ...))]
      [(_ (defd:id param:id ... . rest:id) defn ...)
       #:with defd-head-label
       (datum->syntax #'defd (symbol->string (syntax-e #'defd)))
       #'(define (defd param ... . rest)
           ; Sometimes types in rest aren't be parsed, crashing label ...
           (define defd-label
             (if (or (not (procedure? param)) ...)
                 "???"
                 (type-con-label (list* defd-head-label
                                        (type-label param) ...
                                        (map type-label rest)))))
           (labeled-type (λ () defn ...) defd-label))]))
  )

#;(define-docs define-type/syntax
    [Syntax: (define-type/syntax (defd:id x ...) #:no-label defn)]
    [Semantics: #<<"
Creates a new parameterized type macro, @defd, so that @(defd x ...) expands
into @defn.
"
                ])
(define-syntax define-type/syntax
  (syntax-parser
    [(_ x ...) #'(begin-for-syntax (define-type/syntax x ...))]))

(begin-for-syntax
  ; See phase 0 definition for docs
  (define-syntax define-type/syntax
    (syntax-parser
      [(_ (defd:id x ...) #:no-label defn)
       #'(define-syntax defd
           (syntax-parser
             [(_ x ...) #`(λ () #,defn)]))]))

  #;(define-docs (type-con-label sub-labels)
      [Signature: [Listof String] -> String]
      [Purpose: #<<"
Creates a label for a type constructor given the labels for its arguments.
"
                ]
      [Examples:
       (type-con-label (list "Fooof" "Bar" "Baz")) => "[Fooof Bar Baz"])
  (define (type-con-label sub-labels)
    (string-join sub-labels " " #:before-first "[" #:after-last "]")))