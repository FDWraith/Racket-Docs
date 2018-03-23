#lang racket

(provide [struct-out accumulator])

#;(define-data Accumulator
    [: (accumulator Syntax Desc)]
    [Interpretation: "Encodes an accumulator statement"]
    [Examples:
     "Accumulator: acc : The elements in x0 not in x, already reversed." <=
     (accumulator #'acc "The elements in x0 not in x, already reversed.")])
(struct accumulator [id desc])