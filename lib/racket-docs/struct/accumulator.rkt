#lang typed/racket

(provide [struct-out accumulator])

(struct accumulator [(id : Syntax) (desc : String)] #:transparent)