#lang racket

(provide [struct-out doc-prop]
         type-doc-prop
         sig-doc-prop
         desc-doc-prop
         purpose-doc-prop
         interpretation-doc-prop
         examples-doc-prop
         accumulator-doc-prop
         generative-doc-prop
         effects-doc-prop
         prop-type=?
         prop-type->string
         check-shared-types)

(struct doc-prop [type value] #:transparent)

(define (type-doc-prop type)
  (doc-prop 'type type))

(define sig-doc-prop type-doc-prop)

(define (desc-doc-prop desc)
  (doc-prop 'desc desc))

(define purpose-doc-prop desc-doc-prop)

(define interpretation-doc-prop desc-doc-prop)

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

(define (check-shared-types props)
  (check-duplicates (map doc-prop-type props)))