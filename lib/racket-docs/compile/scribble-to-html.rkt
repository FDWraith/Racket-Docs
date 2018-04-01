#lang racket

(provide compile-docs)

(require raco/all-tools)

(define scribble-spec (hash-ref (all-tools) "scribble"))

(define (run-scribble args)
  (parameterize [(current-command-line-arguments args)]
    (dynamic-require (second scribble-spec) #false)))

; Compiles the scribble file into HTML.
; If #:delete is #true, deletes the scribble file afterward.
(define (compile-docs [name "temp"] #:delete delete?)
  (define path (string-append name ".scrbl"))
  (run-scribble (vector "--htmls" path))
  (when delete?
    (delete-file path)))
