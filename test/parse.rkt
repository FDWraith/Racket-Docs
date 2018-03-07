#lang typed/racket

(require [for-syntax "../lib/racket-docs/struct.rkt"
                     "../lib/racket-docs/utils.rkt"
                     typed/racket/no-check]
         "../lib/racket-docs/parse.rkt"
         "../lib/racket-docs/utils.rkt"
         typed/rackunit
         typed/racket/unsafe)
; Needs to be unsafe - otherwise it would say "higher-order value can't be Any"
(unsafe-require/typed "../lib/racket-docs/utils.rkt"
                      [equal-datum? [Any Any -> Boolean]])

(define-data Foo
  [: Any]
  [Interpretation: "Anything"]
  [Examples:
   5
   "H" <= "\"H\""
   7
   '() <= "Empty List"])

(define-data Bar
  [: - Integer
     - String ]
  [Interpretation: "An integer or string"]
  [Examples:
   5
   "H" <= "\"H\""])

(define-docs foo
  [Signature: Integer]
  [Purpose: "Something"])
(define foo 1)

(define-docs (foo-cons foo foos)
  [Signature: Foo [Listof Foo] -> Foo]
  [Purpose: "Prepends @foo onto @foos"]
  [Examples:
   (foo-cons 1 '()) => '(1)
   (foo-cons "Hello" '("World" "!")) => '("Hello" "World" "!")]
  [Effects: "No effects"])
(define (foo-cons x y)
  (error "Not implemented"))

(define-for-syntax expected-docs
  (list
   (doc-entry
    #'Foo
    (list
     (doc-prop 'type #'Any)
     (doc-prop 'desc "Anything")
     (doc-prop
      'examples
      (list
       (plain-data-example #'5)
       (interpret-data-example #'"H" "\"H\"")
       (plain-data-example #'7)
       (interpret-data-example #''() "Empty List")))))
   (doc-entry
    #'Bar
    (list
     (doc-prop 'type #'[U Integer String])
     (doc-prop 'desc "An integer or string")
     (doc-prop
      'examples
      (list
       (plain-data-example #'5)
       (interpret-data-example #'"H" "\"H\"")))))
   (doc-entry
    #'foo
    (list
     (doc-prop 'type #'Integer)
     (doc-prop 'desc "Something")))
   (doc-entry
    #'foo-cons
    (list
     (doc-prop 'type #'[Foo [Listof Foo] -> Foo])
     (doc-prop 'desc "Prepends @foo onto @foos")
     (doc-prop 'examples
               (list (eval-example #'(foo-cons 1 '()) #''(1))
                     (eval-example #'(foo-cons "Hello" '("World" "!"))
                                   #''("Hello" "World" "!"))))
     (doc-prop 'effects "No effects")))))

(define-syntax get-all-expected-docs
  (mk-id-macro #`'#,(datum->syntax #false expected-docs)))

(check
 equal-datum?
 get-all-docs
 get-all-expected-docs)
