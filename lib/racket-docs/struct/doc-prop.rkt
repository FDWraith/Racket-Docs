#lang typed/racket

(provide [struct-out doc-prop]
         type-doc-prop
         sig-doc-prop
         syntax-doc-prop
         desc-doc-prop
         interpretation-doc-prop
         purpose-doc-prop
         semantics-doc-prop
         examples-doc-prop
         accumulator-doc-prop
         generative-doc-prop
         effects-doc-prop
         prop-type=?
         prop-type->string
         check-shared-types)

(define-type Doc-Prop-Type
  [U 'type
     'syntax
     'desc
     'examples
     'accumulator
     'generative
     'effects])

(struct doc-prop [(type : Doc-Prop-Type) (value : Any)] #:transparent)

(define (type-doc-prop type)
  (doc-prop 'type type))

(define sig-doc-prop type-doc-prop)

(define (syntax-doc-prop syntax)
  (doc-prop 'syntax syntax))

(define (desc-doc-prop desc)
  (doc-prop 'desc desc))

(define interpretation-doc-prop desc-doc-prop)

(define purpose-doc-prop desc-doc-prop)

(define semantics-doc-prop desc-doc-prop)

(define (examples-doc-prop examples)
  (doc-prop 'examples examples))

(define (accumulator-doc-prop accumulator)
  (doc-prop 'accumulator accumulator))

(define (generative-doc-prop generative)
  (doc-prop 'generative generative))

(define (effects-doc-prop effects)
  (doc-prop 'effects effects))

(define prop-type=? symbol=?)

(define prop-type->string symbol->string)

(: check-shared-types : [Listof doc-prop] -> (U Doc-Prop-Type #false))
(define (check-shared-types props)
  (check-duplicates (map doc-prop-type props)))