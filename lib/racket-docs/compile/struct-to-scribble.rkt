#lang racket

(require  "../struct.rkt"
          "../types.rkt"
          "../utils.rkt"
          "../parse.rkt")

(provide compile-docs)

; Effect: Generates a Scribble File from the documentation
(define (compile-docs docs [name "temp"])
  (define path (string-append name ".scrbl"))
  (define (mk-ent-type? type)
    (λ (ent) (symbol=? (doc-entry-type ent) type)))
  (define (mk-ent-string type)
    (string-join (map compile-doc-entry (filter (mk-ent-type? type) docs))
                 "\n"))
  (define section-tags
    (list "datadef"
          "constdef"
          "funcdef"
          "macrodef"))
  (define section-labels
    (list "Data Definitions"
          "Constant Definitions"
          "Function Definitions"
          "Macro Definitions"))
  (define section-types
    (list 'type 'const 'func 'macro))
  (define out-sections
    (map (λ (tag label type)
           (string-append "@section[#:tag \""
                          tag
                          "\"]{"
                          label
                          "}\n"
                          (mk-ent-string type)))
         section-tags
         section-labels
         section-types))
  (define out-string (string-join out-sections "\n"))
  (define out (open-output-file path #:exists 'truncate))
  (display "#lang scribble/manual\n\n" out)
  (fprintf out "@title{~a Documentation}\n" (string-titlecase name))
  (for ([tag section-tags])
    (fprintf out "@secref{~a}\n\n" tag))
  (display "\n" out)
  (display out-string out)
  (close-output-port out))

; Compiles Doc Entries to valid Scribble line(s)
(define (compile-doc-entry ent)
  (define ent-type (doc-entry-type ent))
  (cond
    [(symbol=? ent-type 'type) (compile-doc-data ent)]
    [(symbol=? ent-type 'func) (compile-doc-func ent)]
    [(symbol=? ent-type 'const) (compile-doc-const ent)]
    [(symbol=? ent-type 'macro) (compile-doc-macro ent)]))

; Compiles Data Definitions to valid Scribble line(s)
(define (compile-doc-data dat)
  (define dat-type (syntax->string (doc-entry-id dat)))
  (define props (doc-entry-props dat))
  (define type-prop (extract (mk-prop? 'type) props))
  (define type (doc-prop-value type-prop))
  (define type-string (format "(code:line ~a)" (string-join (type-label/union type) "\n")))
  (define desc-prop (extract (mk-prop? 'desc) props))
  (define desc (doc-prop-value desc-prop))  
  (define example-prop (extract (mk-prop? 'examples) props))
  (define examples (if (empty? example-prop) ""
                       (compile-doc-examples (doc-prop-value example-prop))))
  (string-append "@defthing[#:kind \"data defintion\" #:link-target? #f "
                 dat-type
                 " Type #:value "
                 type-string
                 "]{\n"
                 desc "\n"
                 "}\n\n"
                 examples "\n"))

; Compiles Identifiers to valid Scribble line(s)
(define (compile-doc-const ent)
  (define name (syntax->string (doc-entry-id ent)))
  (define props (doc-entry-props ent))
  (define desc-prop (extract (mk-prop? 'desc) props))
  (define type-prop (extract (mk-prop? 'type) props))
  (define type (type-label (doc-prop-value type-prop)))
  (define desc (doc-prop-value desc-prop))
  (define example-prop (extract (mk-prop? 'examples) props))
  (define examples (if (empty? example-prop) ""
                       (compile-doc-examples (doc-prop-value example-prop))))
  (string-append "@defthing[#:kind \"constant\" #:link-target? #f "
                 name
                 " "
                 type
                 "]{\n"
                 desc
                 "\n}\n\n"
                 examples "\n"))

; Compiles Functions to valid Scribble line(s)
(define (compile-doc-func ent)
  (define name (syntax->string (doc-entry-id ent)))
  (define props (doc-entry-props ent))
  (define args-prop (extract (mk-prop? 'args) props))
  (define args (rest (map syntax->string (syntax->list (doc-prop-value args-prop)))))
  (define type-prop (extract (mk-prop? 'type) props))
  (define types (try-func-params (doc-prop-value type-prop)))
  (define args-info
    (and types
         (map (λ (arg type) (string-append "[" arg " " (type-label type) "]"))
              args types)))
  (define args-string
    (if args-info
        (string-join args-info " ")
        "??? ..."))
  (define out-type (try-func-out (doc-prop-value type-prop)))
  (define output
    (if out-type
        (type-label out-type)
        "???"))
  (define desc-prop (extract (mk-prop? 'desc) props))
  (define purp (doc-prop-value desc-prop))
  (define example-prop (extract (mk-prop? 'examples) props))
  (define examples (if (empty? example-prop) ""
                       (compile-doc-examples (doc-prop-value example-prop))))
  (string-append "@defproc[#:link-target? #f ("
                 name " "
                 args-string ") "
                 output "]{\n"
                 purp "\n"
                 "\n}\n\n"
                 examples "\n"))

; Compiles Macros to valid Scribble line(s)
(define (compile-doc-macro ent)
  (define name (syntax->string (doc-entry-id ent)))
  (define props (doc-entry-props ent))
  (define stx-prop (extract (mk-prop? 'syntax) props))
  (define sem-prop (extract (mk-prop? 'desc) props))
  (define stx (syntax->string (doc-prop-value stx-prop)))
  (define sem (doc-prop-value sem-prop))
  (string-append "@defthing[#:kind \"Syntax\" #:link-target? #f "
                 name
                 "]{\n"
                 sem 
                 "\n}\n"))

; Compiles Examples
(define (compile-doc-examples loe)
  (define (compile-example ex)
    (cond
      [(plain-data-example? ex)
       (format "@racketblock[~a] \n"
               (syntax->string (plain-data-example-expr ex)))]
      [(interpret-data-example? ex)
       (format "@racketblock[~a] can be interpreted as @racketblock[~a]. \n"
               (syntax->string (interpret-data-example-expr ex))
               (syntax->string (interpret-data-example-interpretation ex)))]
      [(eval-example? ex)
       (format "@racketblock[~a] evaluates to @racketblock[~a]. \n"
               (syntax->string (eval-example-expr ex))
               (syntax->string (eval-example-expected ex)))]))
  (string-append
   "@bold{Examples:} \n\n"
   (string-join (map (λ (ex) (format "~a \n " (compile-example ex))) loe) "")))