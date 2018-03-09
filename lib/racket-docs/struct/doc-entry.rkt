#lang racket ; typed-racket causes weird errors for syntax docs

(require "doc-prop.rkt")

(provide [struct-out doc-entry])

(struct doc-entry [id props] #:transparent)