#lang racket-docs

(begin-for-syntax
  (let [] ; Prevents these definitions from being used in the injected file.
    #;(define-docs file-name
        [Signature: String]
        [Purpose: "The name of this file."])
    (define source-name "#$source-name#$")
    #;(define-docs (stop msg)
        [Signature: String -> Nothing]
        [Purpose: "Exits the program, displaying the given message."])
    (define (stop msg)
      (parameterize
          [(error-display-handler
            (λ (msg fake-error)
              (displayln msg)))]
        (error msg)))

    (unless (no-docs?)
      (displayln "Compiling docs ...")
      (compile-docs (get-all-docs) source-name)
      (stop "Successfully compiled docs."))))