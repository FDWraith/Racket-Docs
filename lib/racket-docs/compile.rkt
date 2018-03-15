#lang typed/racket

;; Figure out where this compile-time function is being called from ???
(define out (open-output-file "hello.scrbl"))
(write "hello world" out)
(close-output-port out)


; Effect: Generates a Scrible File from the documentation
(define (compile-docs all-docs)
  (local
    ((define out (open-output-file "hello.scrbl"))
     (define (print/s arg) (print arg out)))
    (map (compose print/s compile-doc-entry) all-docs))))


; Doc-Entry -> String
; Compiles the document entry to valid Scribble line(s)
(define (compile-doc-entry ent)
  (cond
    [(doc-data? ent) (compile-doc-data ent)]
    [(doc-func? ent) (compile-doc-func ent)]))

; Doc-Data -> String
; Compiles Data Definitions to valid Scribble line(s)
(define (compile-doc-data dat)
  (local
    ((define dat-type (doc-entry-id dat))
     (define dat-body (doc-entry-props dat)))
    ...))
    


