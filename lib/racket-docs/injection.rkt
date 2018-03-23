#lang racket-docs
(begin-for-syntax
  #;(define-docs (stop msg)
      [Signature: String -> Nothing]
      [Purpose: "Exits the program, displaying the given message."])
  (define (stop msg)
    (parameterize
      [(error-display-handler
        (λ (msg fake-error)
          (displayln msg)))]
     (error msg)))
  
  (displayln "Compiling docs ...")

  (compile-docs)
  
  (stop "Successfully compiled docs."))