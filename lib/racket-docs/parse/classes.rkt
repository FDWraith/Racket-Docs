#lang racket

(provide head
         union-type
         type
         raw-text
         extra-doc-prop
         extra-data-doc-prop)

(require "raw-text.rkt"
         "../struct.rkt"
         "../utils/syntax.rkt"
         "../utils/parse-class.rkt"
         syntax/parse
         [for-template "../types.rkt"])

(define-syntax-class head
  [pattern id:id
           #:attr args #false
           #:attr args-pure #false]
  [pattern (id:id arg:id ...)
           #:attr args #'(list arg ...)
           #:attr args-pure #'(arg ...)])

(define-splicing-parse-class union-type
  #:datum-literals (-)
  [(~seq (~seq - ~! sub-type:type) ...+)
   (datum->syntax (first this-syntax) [cons 'Union #'(sub-type.out ...)])]
  [x:type #'x.out])

(define-splicing-parse-class type
  [x:unparsed-type (parse-type #'x.out)])

(define-splicing-parse-class unparsed-type
  #:datum-literals (All ->)
  [(~seq {All x:id} ~! f:unparsed-type)
   #'[Forall x f.out]]
  [(~seq {All x:id y:id ...} ~! f:unparsed-type)
   #:with f+:non-inline-type #'[{All y ...} f]
   #'[Forall x f+.out]]
  [(~seq i:non-inline-type ... -> o:unparsed-type)
   #'[-> i.out ... o.out]]
  [(~seq i:non-inline-type ... -> o:non-inline-type ...)
   #'[-> i.out ... (values o.out ...)]]
  [x:non-inline-type #'x.out]
  [(~seq x:non-inline-type y:non-inline-type z:non-inline-type ...)
   #:fail-when #true #<<"
Multiple types separated by spaces.
Examples:
- 'Listof String' (should be '[Listof String]'),
- '{X} (cons X [Listof X])' (should be '{All X} (cons X [Listof X])'),
- '2 2 4' (should be '2 2 -> 4').
"
   #'(void)
   ])

(define-parse-class non-inline-type
  #:datum-literals (All ->)
  [[{All x:id} ~! f:type]
   #'[Forall x f.out]]
  [[{All x:id y:id ...} ~! f:type]
   #:with f+:non-inline-type #'[{All y ...} f]
   #'[Forall x f+.out]]
  [[i:non-inline-type ... -> o:type]
   #'[-> i.out ... o.out]]
  [[i:non-inline-type ... -> o:non-inline-type ...]
   #'[-> i.out ... (values o.out ...)]]
  [[x:non-inline-type ...] #'[x.out ...]]
  [x #'x])

(define-parse-class raw-text
  [str:string (parse-raw-text (syntax-e #'str))])

(define-parse-class extra-doc-prop
  #:datum-literals (Examples: Accumulator: Generative: Effects: :)
  [[Examples: ~! ex:example ...]
   (examples-doc-prop (parse-classes (ex ...)))]
  [[Accumulator: ~! acc:id : desc:raw-text]
   (accumulator-doc-prop (accumulator #'acc (parse-class desc)))]
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