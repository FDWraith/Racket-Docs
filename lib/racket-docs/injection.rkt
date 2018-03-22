#lang racket
(begin-for-syntax
  (displayln "Compiling docs ...")

  ; TODO Replace with method to compile docs.
  (println get-all-docs)
  
  (parameterize
      [(error-display-handler
        (λ (msg fake-error)
          (displayln msg)))]
    (error "Successfully compiled docs.")))