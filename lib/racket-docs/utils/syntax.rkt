#lang racket

(provide stxparse-option
         pattern-option
         temp-matches
         mk-id-macro
         map/stx
         flatten/stx
         equal-datum?)

(require [for-syntax syntax/parse]
         syntax/parse)

; Matches an option before the cases in syntax-parse, syntax-parser, etc.
; Example: #:datum-literals (foo bar baz)
(define-splicing-syntax-class stxparse-option
  [pattern (~seq #:description desc)]
  [pattern (~seq #:datum-literals literals)])

; Matches an option in a case inside syntax-parse, syntax-parser, etc.
; Example: #:attr foo baz
(define-splicing-syntax-class pattern-option
  [pattern (~seq #:with bind val)]
  [pattern (~seq #:attr bind val)]
  [pattern (~seq #:fail-when pred reason)]
  [pattern (~seq #:fail-unless pred reason)]
  [pattern (~seq #:when pred)])

; Matches the given identifier or the wildcard _.
(define-syntax-class (temp-matches id)
  #:description (format "~a or _" (syntax-e id))
  [pattern (~datum _)]
  [pattern case-id:id
           #:when (symbol=? (syntax-e #'case-id) (syntax-e id))])
          

; SYNTAX: (mk-id-macro expr ... stx)
; Creates a syntax parser which only recognizes identifiers. When used in a
; define-syntax, it will replace what's defined with the given syntax.
(define-syntax mk-id-macro
  (syntax-parser
    [(_ expr ... stx)
     #'(syntax-parser [_:id expr ... stx])]))

; {X Y} [X -> Y] [Stx [Listof X]] -> [Stx [Listof Y]]
; Transforms the list within the syntax.
(define (map/stx f stx)
  (datum->syntax stx (map f (syntax->list stx))))

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