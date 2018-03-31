#lang racket

(require ;[prefix-in html. "compile/html.rkt"]
         [prefix-in scribble. "compile/scribble.rkt"])

(provide compile-docs)

; Effect: Generates a folder with the documentation
(define (compile-docs docs [path "temp"])
  (scribble.compile-docs docs path)
  #;(html.compile-docs path #:delete #true))