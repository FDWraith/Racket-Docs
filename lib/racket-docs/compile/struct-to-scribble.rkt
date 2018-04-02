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
  (define type-string (wrap-safe (string-join (type-label/union type) "\n")))
  (define desc-prop (extract (mk-prop? 'desc) props))
  (define desc (doc-prop-value desc-prop))
  (define example-prop (extract (mk-prop? 'examples) props))
  (string-append "@defthing[#:kind \"data defintion\" #:link-target? #f "
                 dat-type
                 " Type #:value "
                 type-string
                 "]{\n"
                 desc
                 "\n}\n"))

; Compiles Identifiers to valid Scribble line(s)
(define (compile-doc-const ent)
  (define name (syntax->string (doc-entry-id ent)))
  (define props (doc-entry-props ent))
  (define desc-prop (extract (mk-prop? 'desc) props))
  (define type-prop (extract (mk-prop? 'type) props))
  (define type (safe-type-label (doc-prop-value type-prop)))
  (define desc (doc-prop-value desc-prop))
  (string-append "@defthing[#:kind \"constant\" #:link-target? #f "
                 name
                 " "
                 type
                 "]{\n"
                 desc
                 "\n}\n"))

; Compiles Functions to valid Scribble line(s)
(define (compile-doc-func ent)
  (define name (syntax->string (doc-entry-id ent)))
  (define props (doc-entry-props ent))
  (define args-prop (extract (mk-prop? 'args) props))
  (define args (rest (map syntax->string (syntax->list (doc-prop-value args-prop)))))
  (define type-prop (extract (mk-prop? 'type) props))
  (define func-type (fill-forall (doc-prop-value type-prop)))
  (define param-types (try-func-params func-type))
  (define param-type-labels (map safe-type-label param-types))
  (define args-info
    (and param-types
         (map (λ (arg type-label) (string-append "[" arg " " type-label "]"))
              (pad-right args "???" (length param-type-labels))
              (pad-right param-type-labels "???" (length args)))))
  (define args-string
    (if args-info
        (string-join args-info " ")
        ""))
  (define out-type (try-func-out func-type))
  (define output
    (if out-type
        (safe-type-label out-type)
        "???"))
  (define desc-prop (extract (mk-prop? 'desc) props))
  (define purp (doc-prop-value desc-prop))
  (string-append "@defproc[#:link-target? #f (" name " " args-string ") " output "]{\n" purp "\n}\n"))

; Compiles Macros to valid Scribble line(s)
(define (compile-doc-macro ent)
  (define name (syntax->string (doc-entry-id ent)))
  (define props (doc-entry-props ent))
  (define stx-prop (extract (mk-prop? 'syntax) props))
  (define sem-prop (extract (mk-prop? 'desc) props))
  "another string heree")

; Compiles Examples
(define (compile-examples loe)
  (define compile-example
    (λ (ex)
      (cond
        [(plain-data-example? ex) ""]
        [(interpret-data-example? ex) ""]
        [(eval-example? ex) ""])))
  "")

; Type -> String
; The label of a type, wrapped in (code:line ...),
; so it will be considered a single argument even if the type has spaces.
(define (safe-type-label x)
  (wrap-safe (type-label x)))

; String -> String
; Makes the string considered a single argument even if it has spaces.
(define (wrap-safe x)
  (format "(code:line ~a)" x))