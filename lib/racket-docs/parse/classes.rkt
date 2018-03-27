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
         [for-template "../types.rkt"])

(define-syntax-class head
  [pattern id:id
           #:attr args #false]
  [pattern (id:id arg:id ...)
           #:attr args #'(arg ...)])

(define-splicing-parse-class union-type
  #:datum-literals (-)
  [(~seq (~seq - ~! sub-type:type) ...+)
   #`[Union #,@(parse-classes (sub-type ...))]]
  [x:type (parse-class x)])

(define-splicing-parse-class type
  #:datum-literals (->)
  [(~seq i:non-func-type ... -> o:type)
   #'[-> i.out ... o.out]]
  [(~seq i:non-func-type ... -> o:non-func-type ...)
   #'[-> i.out ... (values o.out ...)]]
  [x:non-func-type #'x.out])

(define-parse-class non-func-type
  #:datum-literals (->)
  [[i:non-func-type ... -> o:type]
   #'[-> i.out ... o.out]]
  [[i:non-func-type ... -> o:non-func-type ...]
   #'[-> i.out ... (values o.out ...)]]
  [[x:non-func-type ...] #'[x.out ...]]
  [x #'x])

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
   #:with this
   (datum->syntax #'(expr => expected)
                  (list #'expr #'=> #'expected)
                  (list (syntax-source #'expr)
                        (syntax-line #'expr)
                        (syntax-column #'expr)
                        (syntax-position #'expr)
                        (+ (syntax-span #'expected)
                           (- (syntax-position #'expected)
                              (syntax-position #'expr)))))
   (eval-example #'expr #'expected #'this)]
  [x (raise-syntax-error 'example-parser #<<"
Not a valid value or syntax example.
Valid examples are of the form x => y, where x and y are expressions.
Note that data definition examples can be x or x <= 5,
but not value or syntax examples.
"
                         #'x)])

(define-splicing-parse-class data-example
  #:datum-literals (<=)
  [(~seq expr <= interpretation:raw-text)
   (interpret-data-example #'expr (parse-class interpretation))]
  [(~seq expr <= bad-interpretation)
   (raise-syntax-error 'example-parser #<<"
Not a valid data example - interpretation should be raw text.
"
                       #'[expr <= bad-interpretation]
                       #'bad-interpretation)]
  [expr (plain-data-example #'expr)])