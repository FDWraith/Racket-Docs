#lang racket

(require  "struct.rkt"
          "types.rkt"
          "utils.rkt"
          "parse.rkt")

(provide compile-docs)

; Effect: Generates a Scribble File from the documentation
(define (compile-docs docs [path "temp.scrbl"])
  (define data? (λ (ent) (symbol=? (doc-entry-type ent) 'type)))
  (define func? (λ (ent) (symbol=? (doc-entry-type ent) 'func)))
  (define const? (λ (ent) (symbol=? (doc-entry-type ent) 'const)))
  (define macro? (λ (ent) (symbol=? (doc-entry-type ent) 'macro)))
  (define data-entries (filter data? docs))
  (define func-entries (filter func? docs))
  (define const-entries (filter const? docs))
  (define macro-entries (filter macro? docs))
  (define data-string (string-join (map compile-doc-data data-entries) "\n"))
  (define func-string (string-join (map compile-doc-func func-entries) "\n"))
  (define const-string (string-join (map compile-doc-const const-entries) "\n"))
  (define macro-string (string-join (map compile-doc-macro macro-entries) "\n"))
  (define out (open-output-file path #:exists 'error))
  (display "#lang scribble/manual\n\n" out)
  (display "@title{Racket Docs}\n\n" out)
  (display "@section{Data Definitions}\n" out)
  (display (string-append data-string "\n\n") out)
  (display "@section{Constant Definitions}\n" out)
  (display (string-append const-string "\n\n") out)
  (display "@section{Function Definitions}\n" out)
  (display (string-append func-string "\n") out)
  (display macro-string out)
  (close-output-port out))

; Compiles Data Definitions to valid Scribble line(s)
(define (compile-doc-data dat)
  (define dat-type (syntax->string (doc-entry-id dat)))
  (define props (doc-entry-props dat))
  (define type-prop (extract (mk-prop? 'type) props))
  (define type (doc-prop-value type-prop))
  (define type-string (string-append "(code:line " (string-join (type-label/basic type) "\n") ")\n"))
  (define desc-prop (extract (mk-prop? 'desc) props))
  (define desc (doc-prop-value desc-prop))
  (define example-prop (extract (mk-prop? 'examples) dat-body))
  (string-append "@defthing[ #:kind \"Data Defintion\" " dat-type " " dat-type"? "  
                 "#:value " type-string "]{\n"
                 desc "\n}\n"))

; Compiles Functions to valid Scribble line(s)
(define (compile-doc-func ent)
  (define name (syntax->string (doc-entry-id ent)))
  (define props (doc-entry-props ent))
  (define args-prop (extract (mk-prop? 'args) props))
  (define args (rest (map syntax->string (syntax->list (doc-prop-value args-prop)))))
  (define type-prop (extract (mk-prop? 'type) props)) 
  (define types (func-params ((doc-prop-value type-prop))))
  (define args-info (map (λ (arg type) (string-append "[" arg " " (type-label type) "]")) args types))
  (define args-string (string-join args-info " "))
  (define output (type-label (func-out ((doc-prop-value type-prop)))))
  (define desc-prop (extract (mk-prop? 'desc) props))
  (define purp (doc-prop-value desc-prop))
  (string-append "@defproc[(" name " " args-string ") " output " ]{\n" purp "}\n"))

; Compiles Identifiers to valid Scribble line(s)
(define (compile-doc-const ent)
  (define name (syntax->string (doc-entry-id ent)))
  (define props (doc-entry-props ent))
  (define desc-prop (extract (mk-prop? 'desc) props))
  (define type-prop (extract (mk-prop? 'type) props))
  (define type (type-label (doc-prop-value type-prop)))
  (define desc (doc-prop-value desc-prop))
  (string-append "@defthing[ #:kind \"Constant\"" name " " type"]{\n" desc "}\n"))

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