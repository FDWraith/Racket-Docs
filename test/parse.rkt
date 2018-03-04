#lang turnstile

(require [for-syntax "../lib/struct.rkt"
                     "../lib/utils.rkt"]
         "../lib/parse.rkt"
         "../lib/utils.rkt"
         rackunit)

(define-base-type Foo)
(define-type-constructor -> #:arity > 0)
(define-type-constructor Listof #:arity = 1)

(define-data Foo
  [: Foo]
  [Interpretation: "Anything"]
  [Examples:
   5
   "H" <= "\"H\""
   7
   '() <= "Empty List"])

(define-docs (foo-cons foo foos)
  [Signature: [-> Foo [Listof Foo] Foo]]
  [Purpose: "Prepends @foo onto @foos"]
  [Examples:
   (foo-cons 1 '()) => '(1)
   (foo-cons "Hello" '("World" "!")) => '("Hello" "World" "!")]
  [Effects: "No effects"])

(begin-for-syntax
  (define expected-docs
    (list
     (doc-entry
      #'foo-cons
      (list
       (doc-prop 'type #'[-> Foo [Listof Foo] Foo])
       (doc-prop 'desc "Prepends @foo onto @foos")
       (doc-prop 'examples
                 (list (eval-example #'(foo-cons 1 '()) #''(1))
                       (eval-example #'(foo-cons "Hello" '("World" "!"))
                                     #''("Hello" "World" "!"))))
       (doc-prop 'effects "No effects")))
     (doc-entry
      #'Foo
      (list
       (doc-prop 'type #'Foo)
       (doc-prop 'desc "Anything")
       (doc-prop
        'examples
        (list
         (plain-data-example #'5)
         (interpret-data-example #'"H" "\"H\"")
         (plain-data-example #'7)
         (interpret-data-example #''() "Empty List"))))))))

(define-syntax get-all-expected-docs
  (mk-id-macro #`'#,(datum->syntax #false expected-docs)))

(check
 equal-datum?
 get-all-docs
 get-all-expected-docs)
