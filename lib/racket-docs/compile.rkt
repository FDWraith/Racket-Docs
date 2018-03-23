#lang racket

(require (for-syntax "struct.rkt"
                     "utils.rkt"
                     "parse.rkt"
                     racket
                     syntax/parse))

(provide compile-docs)

; Effect: Generates a Scribble File from the documentation
(define-syntax (compile-docs stx)
  (syntax-parse stx
    [(_ docs path)
     (define entries (map compile-doc-entry (eval #'docs)))
     (define ent-string (string-join entries "\n"))
     #`(begin
         (define out (open-output-file path #:exists 'truncate/replace))
         (display "#lang scribble/manual\n" out)
         (display #,ent-string out)
         (close-output-port out))]))

; Compiles the document entry to valid Scribble line(s)
(define-for-syntax (compile-doc-entry ent)
  (begin
    (define ent-type (doc-entry-type ent))
    (define compiled (cond
                       [(symbol=? ent-type 'type) (compile-doc-data ent)]
                       [(symbol=? ent-type 'value) (compile-doc-const ent)]
                       [(symbol=? ent-type 'macro) (compile-doc-func ent)]))
    compiled))

; Compiles Data Definitions to valid Scribble line(s)
(define-for-syntax (compile-doc-data dat)
  (begin
    (define dat-type (syntax->string (doc-entry-id dat)))
    (define dat-body (doc-entry-props dat))
    (define type-prop? (mk-prop? 'type))
    (define type-prop (extract type-prop? dat-body))
    (define type
      (cond
        [(doc-prop? type-prop) (syntax->string (doc-prop-value type-prop))]
        [else (error "type not found")]))
    (define desc-prop? (mk-prop? 'desc))
    (define desc-prop (extract desc-prop? dat-body))
    (define desc (if (empty? desc-prop)
                     ""
                     (doc-prop-value desc-prop)))
    (define example-prop? (mk-prop? 'examples))
    (define example-prop (extract example-prop? dat-body))
    (string-append "Data:" dat-type "\n"
                   "Types:\n"
                   type "\n"
                   "Interpretation:" desc)))

; Compiles Functions to valid Scribble line(s)
(define-for-syntax (compile-doc-func stx)
  void)

; Compiles Constants to valid Scribble line(s)
(define-for-syntax (compile-doc-const stx)
  void)
     