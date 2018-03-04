#lang racket

(provide head
         union-type
         type
         raw-text
         extra-doc-prop
         extra-data-doc-prop)

(require "../struct.rkt"
         "../utils/syntax.rkt"
         "../utils/parse-class.rkt"
         syntax/parse
         [for-template turnstile])

(define-parse-class head
  [id:id this-syntax]
  [(id:id arg:id ...) this-syntax])

(define-parse-class union-type
  #:datum-literals (-)
  [out:type this-syntax]
  [((~seq - sub-type:type) ...)
   #'[Union sub-type ...]])

;type imported from turnstile

(define-parse-class raw-text
  [str:string (syntax-e #'str)])

(define-parse-class extra-doc-prop
  #:datum-literals (Examples: Accumulator: Generative: Effects: :)
  [[Examples: ~! ex:example ...]
   (examples-doc-prop (parse-classes (ex ...)))]
  [[Accumulator: ~! acc:id : desc:raw-text]
   (accumulator-doc-prop #'acc (parse-class desc))]
  [[Generative: ~! desc:raw-text]
   (generative-doc-prop (parse-class desc))]
  [[Effects: ~! desc:raw-text]
   (effects-doc-prop (parse-class desc))])

(define-parse-class extra-data-doc-prop
  #:datum-literals (Examples:)
  [[Examples: ~! ex:data-example ...]
   (examples-doc-prop (parse-classes (ex ...)))])

(define-splicing-parse-class example
  #:datum-literals (=>)
  [(~seq expr => expected)
   (eval-example #'expr #'expected)])

(define-splicing-parse-class data-example
  #:datum-literals (<=)
  [(~seq expr <= ~! interpretation:raw-text)
   (interpret-data-example #'expr (parse-class interpretation))]
  [expr (plain-data-example #'expr)])