#lang racket

(provide shared-type-err)

(require "../struct.rkt")

(define (shared-type-err shared-type)
  (format "Multiple ~as in a documentation block"
          (prop-type->string shared-type)))