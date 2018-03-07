#lang racket

(provide attributes
         mk-id-macro
         flatten/stx
         equal-datum?)

(require [for-syntax syntax/parse]
         syntax/parse)

(define-syntax (attributes stx)
  (syntax-parse stx
    [(_ (attr (~datum ...))) #'(attribute attr)]))

; SYNTAX: (mk-id-macro expr ... stx)
; Creates a syntax parser which only recognizes identifiers. When used in a
; define-syntax, it will replace what's defined with the given syntax.
(define-syntax mk-id-macro
  (syntax-parser
    [(_ expr ... stx)
     #'(syntax-parser [_:id expr ... stx])]))

; {X} [Stx [Listof [Listof X]]] -> [Stx [Listof X]]
; Flattens the list within the syntax.
(define (flatten/stx stx)
  (datum->syntax stx (foldr append '()
                            (map syntax-e
                                 (syntax-e stx)))))

; Whether the given values are equal, but if both values are syntax objects,
; their syntax information is strict and only their datum values are compared.
(define (equal-datum? x y)
  (cond
    [(and (syntax? x) (syntax? y))
     (equal-datum? (syntax->datum x) (syntax->datum y))]
    [else (equal?/recur x y equal-datum?)]))