#lang s-exp syntax/module-reader
racket-docs

#:info
(λ (keyword racket-default get-default)
  (cond
    [(equal? keyword 'drracket:toolbar-buttons)
     (list compile-docs-button)]
    [else (get-default keyword racket-default)]))

(require racket/draw
         racket/class)
(define compile-docs-label "Compile Docs")
(define compile-docs-icon
  (read-bitmap (open-input-file (collection-file-path "resources/icon16.png"
                                                      "racket-docs"))))
(define (compile-docs ide)
  (send ide execute-callback))
(define compile-docs-priority 99)
(define compile-docs-button
  (list compile-docs-label
        compile-docs-icon
        compile-docs
        compile-docs-priority))