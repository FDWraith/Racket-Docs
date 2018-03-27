#lang racket

(require  "struct.rkt"
          "types.rkt"
          "utils.rkt"
          "parse.rkt")

(provide compile-docs)

; Effect: Generates a Scribble File from the documentation
(define (compile-docs docs [path "temp.scrbl"])
  (begin
    (define data? (λ (ent) (symbol=? (doc-entry-type ent) 'type)))
    (define data-entries (filter data? docs))
    (define func-entries (filter (compose not data?) docs))
    (define data-string (string-join (map compile-doc-entry data-entries) "\n"))
    (define func-string (string-join (map compile-doc-entry func-entries) "\n"))
    (define out (open-output-file path #:exists 'error))
    (display "#lang scribble/manual\n\n" out)
    (display "@title{Racket Docs}\n\n" out)
    (display "@section{Data Definitions}\n" out)
    (display (string-append data-string "\n\n") out)
    (display "@section{Function Definitions}\n" out)
    (display func-string out)
    (close-output-port out)))

; Compiles the document entry to valid Scribble line(s)
(define (compile-doc-entry ent)
  (begin
    (define ent-type (doc-entry-type ent))
    (define compiled (cond
                       [(symbol=? ent-type 'type) (compile-doc-data ent)]
                       [(symbol=? ent-type 'func) (compile-doc-func ent)]
                       [(symbol=? ent-type 'const) (compile-doc-const ent)]
                       [(symbol=? ent-type 'macro) (compile-doc-macro ent)]))
    compiled))

; Compiles Data Definitions to valid Scribble line(s)
(define (compile-doc-data dat)
  (define dat-type (syntax->string (doc-entry-id dat)))
  (define dat-body (doc-entry-props dat))
  (define type-prop? (mk-prop? 'type))
  (define type-prop (extract type-prop? dat-body))
  (define type (doc-prop-value type-prop))
  (define type-info (basic-type-summary type))
  (define type-string (string-append "(code:line " (string-join type-info "\n") ")\n"))
  #;(define type-string (string-append "(code:line " (string-join (map syntax->string (syntax->list type)) " ") ")\n"))
  (define desc-prop? (mk-prop? 'desc))
  (define desc-prop (extract desc-prop? dat-body))
  (define desc (if (empty? desc-prop)
                   ""
                   (doc-prop-value desc-prop)))
  (define example-prop? (mk-prop? 'examples))
  (define example-prop (extract example-prop? dat-body))
  (string-append "@defthing[ #:kind \"Data Defintion\" " dat-type " " dat-type"? "  
                 "#:value " type-string "]{\n"
                 desc "\n}\n"))

; Compiles Functions to valid Scribble line(s)
(define (compile-doc-func ent)
  (define name (syntax->string (doc-entry-id ent)))
  (define props (doc-entry-props ent))
  (define args-prop? (mk-prop? 'args))
  (define args-prop (extract args-prop? props))
  (define args (map/stx (λ (stx) (syntax->string stx)) (doc-prop-value args-prop)))
  (define type-prop? (mk-prop? 'type))
  (define type-prop (extract type-prop? props))
  (define types (doc-prop-value type-prop))
  (println types)
  (define args-info (map (λ (arg type) (string-append "[" arg " " (type-summary type) "]")) args types))
  (define args-string (string-join args-info " "))
  (define desc-prop? (mk-prop? 'desc))
  (define desc-prop (extract desc-prop? props))
  (define purp (doc-prop-value desc-prop))
  "@defproc[(" name " " args-string ")]{\n" purp "}\n")

; Compiles Identifiers to valid Scribble line(s)
(define (compile-doc-const ent)
  (begin
    (define name (syntax->string (doc-entry-id ent)))
    (define props (doc-entry-props ent))
    (define stx-prop? (mk-prop? 'syntax))
    (define stx-prop (extract stx-prop? props))
    (define sem-prop? (mk-prop? 'desc))
    (define sem-prop (extract sem-prop? props))
    "return here"))

; Compiles Macros to valid Scribble line(s)
(define (compile-doc-macro stx)
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