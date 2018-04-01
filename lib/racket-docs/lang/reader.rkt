#lang s-exp syntax/module-reader
racket-docs

#:info
(λ (keyword racket-default get-default)
  (match keyword
    ['drracket:toolbar-buttons (list compile-docs-button)]
    [else (get-default keyword racket-default)]))

(require racket/draw
         racket/class
         racket/match
         racket/port
         racket/path
         racket/function
         racket/string)

(define compile-docs-icon-path
  (collection-file-path "resources/icon16.png" "racket-docs"))
(define docs-injection-path
  (collection-file-path "injection.rkt" "racket-docs"))

(define compile-docs-label "Compile Docs")
(define compile-docs-icon
  (read-bitmap (open-input-file compile-docs-icon-path)))
(define (compile-docs ide)
  (define definitions-text (send ide get-definitions-text))
  (define text (send definitions-text get-text))
  (define source-path (send definitions-text get-port-name))
  (define source-name
    (path->string (file-name-from-path (path-replace-extension source-path
                                                               ""))))
  (define reg-text-port (open-input-string text source-path))
  (define gen-injection-port (open-input-file docs-injection-path))
  (define gen-injection-string (port->string gen-injection-port))
  (define injection-string
    (string-replace gen-injection-string "#$source-name#$" source-name))
  (define injection-port
    (open-input-string injection-string docs-injection-path))
  (read-line injection-port) ; Gets rid of #lang, used for syntax checking.
  (define text-port (input-port-append #true reg-text-port injection-port))
  (port-count-lines! text-port)

  (define interactions-text (send ide get-interactions-text))
  (send interactions-text reset-console)
  (send interactions-text clear-undos)
  (send interactions-text evaluate-from-port text-port #t
        (λ ()
          (send interactions-text stop-colorer #true))))

(define compile-docs-priority 99)
(define compile-docs-button
  (list compile-docs-label
        compile-docs-icon
        compile-docs
        compile-docs-priority))