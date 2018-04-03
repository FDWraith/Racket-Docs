#lang racket

(provide [for-syntax define-docs
                     define-data
                     define-syntax/docs
                     get-all-docs
                     no-docs?]
         define-docs
         define-data
         define-syntax/docs)

(require [for-syntax [for-syntax "parse/gen.rkt"
                                 "parse/tests.rkt"
                                 "parse/classes.rkt"
                                 "utils.rkt"
                                 syntax/parse
                                 racket/base]
                     "parse/gen.rkt"
                     "parse/tests.rkt"
                     "parse/classes.rkt"
                     "parse/errors.rkt"
                     "struct.rkt"
                     "utils.rkt"
                     syntax/parse
                     racket/base]
         "types.rkt")

(begin-for-syntax
  #;(define-docs cur-entries
    [Signature: [Listof doc-entry]]
    [Purpose: "The documentation entries parsed so far, in reverse order"])
  (define cur-entries '())

  #;(define-docs (add-doc! entry caller stx shared-stx)
      [Signature: doc-entry Symbol Syntax Syntax -> (void)]
      [Purpose: #<<"
First checks that @entry doen't have any duplicate types. If it has duplicate
types, will generate a syntax error, blaming @stx and @shared-stx. Then adds
@entry to @cur-entries, tracking it.
"
                ])
  (define (add-doc! entry caller stx shared-stx)
    (define shared-type (check-shared-types (doc-entry-props entry)))
    (when shared-type
      (raise-syntax-error caller
                          (shared-type-err shared-type)
                          stx shared-stx))
    (set! cur-entries (cons entry cur-entries)))

  ; See phase 0 definition for docs
  (define-syntax (define-docs stx)
    (syntax-parse stx
      #:datum-literals (Signature: Purpose: Syntax: Semantics:)
      [(_ head:head
          [Signature: sig:type] ~!
          [Purpose: purpose:raw-text]
          extra-prop:extra-doc-prop ...)
       #:with stx #`#'#,stx
       #:with run-tests
       (or (tests-for-props1 (parse-classes (extra-prop ...))) #'(void))
       #`(begin
           (define entry
             (cond
               [(boolean? #'#,(attribute head.args))
                (const-doc-entry
                 #'head.id
                 (list* (sig-doc-prop sig.out)
                        (purpose-doc-prop purpose.out1)
                        extra-prop.out1 ...))]
               [else
                (func-doc-entry
                 #'head.id
                 (list (args-doc-prop #'#,(attribute head.args))
                       (sig-doc-prop sig.out)
                       (purpose-doc-prop purpose.out1)
                       extra-prop.out1 ...))]))
           (add-doc! entry 'define-docs stx #'(extra-prop ...))
           run-tests)]
      [(_ id:id
          [Syntax: stx-case ...] ~!
          [Semantics: semantics:raw-text]
          extra-prop:extra-doc-prop ...)
       #:with stx #`#'#,stx
       #:with run-tests
       (or (tests-for-props1 (parse-classes (extra-prop ...))) #'(void))
       #'(begin
           (define entry
             (macro-doc-entry #'id
                              (list (syntax-doc-prop #'(stx-case ...))
                                    (semantics-doc-prop semantics.out1)
                                    extra-prop.out1 ...)))
           (add-doc! entry 'define-docs stx #'(extra-prop ...))
           run-tests)]))

  ; See phase 0 definition for docs
  (define-syntax (define-data stx)
    (syntax-parse stx
      #:datum-literals (: Interpretation:)
      [(_ head:head
          [: type:union-type]
          [Interpretation: interpretation:raw-text]
          extra-prop:extra-data-doc-prop ...)
       #:with stx #`#'#,stx
       #:with filled-type (fill-type-args (attribute head.args-pure) #'type.out)
       #:with run-tests
       (or (tests-for-props1 (parse-classes (extra-prop ...))) #'(void))
       #'(begin
           (define entry
             (type-doc-entry
              #'head
              (list (type-doc-prop filled-type)
                    (interpretation-doc-prop interpretation.out1)
                    extra-prop.out1 ...)))
           (add-doc! entry 'define-data stx #'(extra-prop ...))
           (define-type/parsed head type.out)
           run-tests)]))

  ; See phase 0 definition for docs
  (define-syntax define-syntax/docs
    (gen-define-syntax/docs #'define-docs))

  #;(define-docs (get-all-docs)
      [Signature: -> [Listof DocEntry]]
      [Purpose: #<<"
Returns all of the documentation entries as structures,
in the order they were defined in the file.
"
                  ])
  (define (get-all-docs)
    (reverse cur-entries))

  #;(define-docs (no-docs)
      [Signature: -> Bool]
      [Purpose: "Whether there are any docs."])
  (define (no-docs?)
    (null? (get-all-docs))))

#;(define-docs define-docs
  [Syntax:
   (define-docs head:head
     [Signature: sig:type]
     [Purpose: purpose:raw-text]
     extra-prop:extra-doc-prop ...)
   (define-docs id:id
     [Syntax: stx-case ...]
     [Semantics: semantics:raw-text]
     extra-prop:extra-doc-prop ...)]
  [Semantics: #<<"
Documents a function, constant or macro.
If documenting a function, also assignes the documented type.
"
              ])
(define-syntax (define-docs stx)
  (syntax-parse stx
    #:datum-literals (Signature: Purpose: Syntax: Semantics:)
    [(_ head:head
        (~and full-sig [Signature: sig:type])
        [Purpose: purpose:raw-text]
        extra-prop:extra-doc-prop ...)
     (define sig+ (parse-class sig))
     (define extra-props+ (parse-classes (extra-prop ...)))
     (define run-tests (tests-for-props extra-props+))
     (define entry
       (cond
         [(boolean? (attribute head.args))
          (const-doc-entry
           #'head.id
           (list* (sig-doc-prop/stx sig+)
                  (purpose-doc-prop (parse-class purpose))
                  extra-props+))]
         [else
          (func-doc-entry
           #'head.id
           (list* (args-doc-prop (attribute head.args))
                  (sig-doc-prop/stx sig+)
                  (purpose-doc-prop (parse-class purpose))
                  extra-props+))]))
     (add-doc! entry 'define-docs stx #'(extra-prop ...))
     (if run-tests
         #`(begin
             (assign-type/id/parsed/src head.id #,sig+ full-sig)
             #,run-tests)
         #`(assign-type/id/parsed/src head.id #,sig+ full-sig))]
    [(_ id:id
        [Syntax: stx-case ...]
        [Semantics: semantics:raw-text]
        extra-prop:extra-doc-prop ...)
     (define extra-props+ (parse-classes (extra-prop ...)))
     (define run-tests (tests-for-props extra-props+))
     (define entry
       (macro-doc-entry #'id
                        (list* (syntax-doc-prop #'(stx-case ...))
                               (semantics-doc-prop (parse-class semantics))
                               extra-props+)))
     (add-doc! entry 'define-docs stx #'(extra-prop ...))
     (or run-tests #'(void))]))

#;(define-docs define-data
  [Syntax:
   (define-data head:head
     [: type:union-type]
     [Interpretation: interpretation:raw-text]
     extra-prop:extra-data-doc-prop ...)]
  [Semantics: "Documents a data definition, and creates a type for it."])
(define-syntax (define-data stx)
  (syntax-parse stx
    #:datum-literals (: Interpretation:)
    [(_ head:head
        [: type:union-type]
        [Interpretation: interpretation:raw-text]
        extra-prop:extra-data-doc-prop ...)
     (define type+ (parse-class type))
     (define extra-props+ (parse-classes (extra-prop ...)))
     (define run-tests (tests-for-props extra-props+))
     (define filled-type (fill-type-args (attribute head.args-pure) type+))
     (define entry
       (type-doc-entry
        #'head
        (list* (type-doc-prop/stx filled-type)
               (interpretation-doc-prop (parse-class interpretation))
               extra-props+)))
     (add-doc! entry 'define-data stx #'(extra-prop ...))
     (if run-tests
         #`(begin
             (define-type/parsed head #,type+)
             #,run-tests)
         #`(define-type/parsed head #,type+))]))

#;(define-docs define-syntax/docs
  [Syntax:
   (define-syntax/docs id:id
     [Semantics: semantics:raw-text]
     extra-prop:extra-doc-prop ...
     option:stxparse-option ...
     [((~var case-temp-head (temp-matches #'id)) case-temp-part ...)
      case-body ...+] ...+)
   (define-syntax/docs (id:id stx:id)
     [Semantics: semantics:raw-text]
     extra-prop:extra-doc-prop ...
     option:stxparse-option ...
     [(case-temp-head case-temp-part ...)
      case-body ...+] ...+)]
  [Semantics: #<<"
Combines define-syntax, syntax-parse, and define-docs.
Automatically generates SYNTAX documentation from syntax-parse clauses.
"
              ]
  [Examples:
   (define-syntax/docs foo
     [Semantics: "foo"]
     [Examples:
      (foo bar) => 'baz
      (foo (bar 4)) => 4]
     #:datum-literals (bar)
     [(_ bar) #''baz]
     [(_ (bar x:nat)) #'x]) =>
   (begin
     (define-docs foo
       [Syntax: (foo bar)
                (foo (bar x:nat))]
       [Semantics: "foo"]
       [Examples:
        (foo bar) => 'baz
        (foo (bar 4)) => 4])
     (define-syntax foo
       (syntax-parse
           #:datum-literals (bar)
         [(_ bar) #''baz]
         [(_ (bar x:nat)) #'x])))])
(define-syntax define-syntax/docs
  (gen-define-syntax/docs #'define-docs))