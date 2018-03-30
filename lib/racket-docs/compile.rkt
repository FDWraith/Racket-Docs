#lang racket

(require  "struct.rkt"
          "types.rkt"
          "utils.rkt"
          "parse.rkt")

(provide compile-docs)

; Effect: Generates a Scribble File from the documentation
(define (compile-docs docs [path "temp.scrbl"])
  (define (mk-ent-type? type)
    (λ (ent) (symbol=? (doc-entry-type ent) type)))
  (define (mk-ent-string type)
    (λ (type) (string-join (map compile-doc-entry (filter (mk-ent-type? type)) docs)) "\n"))
  (define out-string
    (map (λ (section type)
           (string-append section "\n" (mk-ent-string type) "\n"))
         (list "@section{Data Definitions}\n"
               "@section{Constant Definitions}\n"
               "@section{Function Definitions}\n" "")
         (list 'type 'func 'const 'macro)))
  (define out (open-output-file path #:exists 'error))
  (display "#lang scribble/manual\n\n" out)
  (display "@title{Racket Docs}\n\n" out)
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
  (define type-string (string-append "(code:line " (string-join (type-label/basic type) "\n") ")\n"))
  (define desc-prop (extract (mk-prop? 'desc) props))
  (define desc (doc-prop-value desc-prop))
  (define example-prop (extract (mk-prop? 'examples) props))
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