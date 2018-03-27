#lang racket

(require [for-syntax "../lib/racket-docs/struct.rkt"
                     "../lib/racket-docs/utils.rkt"
                     syntax/parse
                     rackunit
                     racket/list]
         "../lib/racket-docs/parse.rkt"
         "../lib/racket-docs/types.rkt")

(define-data Foo
  [: Any]
  [Interpretation: "Anything"]
  [Examples:
   5
   "H" <= "\"H\""
   7
   '() <= "Empty List"])

(define-data Bar
  [: - Int
     - String ]
  [Interpretation: "An integer or string"]
  [Examples:
   5
   "H" <= "\"H\""])

(define-docs foo
  [Signature: Int]
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
  (cons x y))

(define-docs append-id
  [Syntax: (append-id id:id ...+)]
  [Semantics: "Appends the @id@s together."]
  [Examples:
   (let [(helloworld 5)] (append-id hello world)) => 5
   (let [(hello 5)] (append-id hello)) => 5
   (let [(abc 10)] (append-id a b c)) => 10])
(define-syntax (append-id stx)
  (syntax-parse stx
    [(_ id:id ...+)
     (datum->syntax stx
                    (string->symbol
                     (foldr string-append ""
                            (map symbol->string
                                 (syntax->datum #'(id ...)))))
                    stx
                    stx)]))

(begin-for-syntax
  (define expected-docs
    (list
     (doc-entry
      'type
      #'Foo
      (list
       (doc-prop 'type Any)
       (doc-prop 'desc "Anything")
       (doc-prop
        'examples
        (list
         (plain-data-example #'5)
         (interpret-data-example #'"H" "\"H\"")
         (plain-data-example #'7)
         (interpret-data-example #''() "Empty List")))))
     (doc-entry
      'type
      #'Bar
      (list
       (doc-prop 'type [Union Int String])
       (doc-prop 'desc "An integer or string")
       (doc-prop
        'examples
        (list
         (plain-data-example #'5)
         (interpret-data-example #'"H" "\"H\"")))))
     (doc-entry
      'value
      #'foo
      (list
       (doc-prop 'args #false)
       (doc-prop 'type Int)
       (doc-prop 'desc "Something")))
     (doc-entry
      'value
      #'foo-cons
      (list
       (doc-prop 'args #'(foo foos))
       (doc-prop 'type [-> Foo [Listof Foo] Foo])
       (doc-prop 'desc "Prepends @foo onto @foos")
       (doc-prop 'examples
                 (list (eval-example #'(foo-cons 1 '())
                                     #''(1)
                                     #'[(foo-cons 1 '()) => '(1)])
                       (eval-example #'(foo-cons "Hello" '("World" "!"))
                                     #''("Hello" "World" "!")
                                     #'[(foo-cons "Hello" '("World" "!"))
                                        =>
                                        '("Hello" "World" "!")])))
       (doc-prop 'effects "No effects")))
                                          
     (doc-entry
      'macro
      #'append-id
      (list
       (doc-prop 'syntax #'((append-id id:id ...+)))
       (doc-prop 'desc "Appends the @id@s together.")
       (doc-prop 'examples
                 (list
                  (eval-example
                   #'(let [(helloworld 5)] (append-id hello world))
                   #'5
                   #'[(let [(helloworld 5)] (append-id hello world)) => 5])
                  (eval-example #'(let [(hello 5)] (append-id hello))
                                #'5
                                #'[(let [(hello 5)] (append-id hello)) => 5])
                  (eval-example #'(let [(abc 10)] (append-id a b c))
                                #'10
                                #'[(let [(abc 10)] (append-id a b c))
                                   =>
                                   10])))))))
  
  (unless (empty? (get-all-docs))
    (displayln "Testing ...")
    (check equal-datum?
           (get-all-docs)
           expected-docs)
    (displayln "Tested")))