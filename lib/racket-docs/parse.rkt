#lang racket

(provide [for-syntax define-docs
                     define-data
                     define-syntax/docs]
         define-docs
         define-data
         define-syntax/docs
         get-all-docs)

(require [for-syntax [for-syntax "parse/gen.rkt"
                                 "parse/classes.rkt"
                                 "utils.rkt"
                                 syntax/parse
                                 racket/base]
                     "parse/gen.rkt"
                     "parse/classes.rkt"
                     "parse/errors.rkt"
                     "struct.rkt"
                     "utils.rkt"
                     syntax/parse]
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
       #'(begin
           (define entry
             (val-doc-entry #'head.id
                            (list (sig-doc-prop #'sig.out1)
                                  (purpose-doc-prop purpose.out1)
                                  extra-prop.out1 ...)))
           (add-doc! entry 'define-docs stx #'(extra-prop ...)))]
      [(_ id:id
          [Syntax: stx-case ...] ~!
          [Semantics: semantics:raw-text]
          extra-prop:extra-doc-prop ...)
       #:with stx #`#'#,stx
       #'(begin
           (define entry
             (macro-doc-entry #'id
                              (list (syntax-doc-prop #'(stx-case ...))
                                    (semantics-doc-prop semantics.out1)
                                    extra-prop.out1 ...)))
           (add-doc! entry 'define-docs stx #'(extra-prop ...)))]))

  ; See phase 0 definition for docs
  (define-syntax (define-data stx)
    (syntax-parse stx
      #:datum-literals (: Interpretation:)
      [(_ id:id
          [: type:union-type]
          [Interpretation: interpretation:raw-text]
          extra-prop:extra-data-doc-prop ...)
       #'(begin
           (define entry
             (type-doc-entry
              #'id
              (list* (type-doc-prop #'type.out1)
                     (interpretation-doc-prop interpretation.out1)
                     extra-prop.out1 ...)))
           (add-doc! entry 'define-data stx #'(extra-prop ...)))]))

  ; See phase 0 definition for docs
  (define-syntax define-syntax/docs
    (gen-define-syntax/docs #'define-docs)))

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
Documents a value or macro.
If documenting a value, also assignes the documented type.
"
              ])
(define-syntax (define-docs stx)
  (syntax-parse stx
    #:datum-literals (Signature: Purpose: Syntax: Semantics:)
    [(_ head:head
        [Signature: sig:type]
        [Purpose: purpose:raw-text]
        extra-prop:extra-doc-prop ...)
     (define sig+ (parse-class sig))
     (define entry
       (val-doc-entry #'head.id
                      (list* (sig-doc-prop sig+)
                             (purpose-doc-prop (parse-class purpose))
                             (parse-classes (extra-prop ...)))))
     (add-doc! entry 'define-docs stx #'(extra-prop ...))
     #`(assign-type head.id #,sig+)]
    [(_ id:id
        [Syntax: stx-case ...]
        [Semantics: semantics:raw-text]
        extra-prop:extra-doc-prop ...)
     (define entry
       (macro-doc-entry #'id
                        (list* (syntax-doc-prop #'(stx-case ...))
                               (semantics-doc-prop (parse-class semantics))
                               (parse-classes (extra-prop ...)))))
     (add-doc! entry 'define-docs stx #'(extra-prop ...))
     #'(void)]))

#;(define-docs define-data
  [Syntax:
   (define-data id:id
     [: type:union-type]
     [Interpretation: interpretation:raw-text]
     extra-prop:extra-data-doc-prop ...)]
  [Semantics: "Documents a data definition, and creates a type for it."])
(define-syntax (define-data stx)
  (syntax-parse stx
    #:datum-literals (: Interpretation:)
    [(_ id:id
        [: type:union-type]
        [Interpretation: interpretation:raw-text]
        extra-prop:extra-data-doc-prop ...)
     (define type+ (parse-class type))
     (define entry
       (type-doc-entry
        #'id
        (list* (type-doc-prop type+)
               (interpretation-doc-prop (parse-class interpretation))
               (parse-classes (extra-prop ...)))))
     (add-doc! entry 'define-data stx #'(extra-prop ...))
     #`(define-type id #,type+)])) ; Long-Term TODO: Add type constructors

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

#;(define-docs get-all-docs
  [Syntax: get-all-docs]
  [Semantics: #<<"
Returns all of the documentation entries as structures,
in the order they were defined in the file.
"
              ])
(define-syntax get-all-docs
  (mk-id-macro
   ; cur-entries are ordered from bottom of file to top.
   ; User expects entries to be ordered from top of file to bottom.
   (define all-docs (reverse cur-entries))
   #`'#,(datum->syntax #false all-docs)))