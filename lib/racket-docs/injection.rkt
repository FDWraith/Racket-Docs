#lang racket-docs
(begin-for-syntax
  (displayln "Compiling docs ...")

  (compile-docs get-all-docs)
  
  (parameterize
      [(error-display-handler
        (λ (msg fake-error)
          (displayln msg)))]
    (error "Successfully compiled docs.")))