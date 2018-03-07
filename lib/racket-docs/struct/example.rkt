#lang typed/racket

(provide [struct-out eval-example]
         [struct-out interpret-data-example]
         [struct-out plain-data-example])

; An Example is one of
; - (eval-example Syntax Syntax)
; - (interpret-data-example Syntax RawText)
; - (plain-data-example Syntax)

(struct eval-example
  [(expr : [Syntaxof Any]) (expected : [Syntaxof Any])] #:transparent)
(struct interpret-data-example
  [(expr : [Syntaxof Any]) (interpretation : String)] #:transparent)
(struct plain-data-example [(expr : [Syntaxof Any])] #:transparent)