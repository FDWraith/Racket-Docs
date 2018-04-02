#lang racket

(require [prefix-in scribble->html. "compile/scribble-to-html.rkt"]
         [prefix-in struct->scribble. "compile/struct-to-scribble.rkt"])

(provide compile-docs)

; Effect: Generates a folder with the documentation
(define (compile-docs docs [name "temp"])
  (displayln "Compiling scribble ...")
  (struct->scribble.compile-docs docs name)
  (displayln "Compiled scribble.")
  (displayln "Compiling html ...")
  (scribble->html.compile-docs name #:delete #false)
  (displayln "Compiled html."))