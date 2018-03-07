#lang racket

(provide [struct-out doc-entry])

(struct doc-entry [id props] #:transparent)