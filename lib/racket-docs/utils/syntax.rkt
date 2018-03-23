#lang racket

(provide stxparse-option
         pattern-option
         temp-matches
         mk-id-macro
         map/stx
         flatten/stx
         equal-datum?
         syntax->string
         extract
         mk-prop?)

(require [for-syntax syntax/parse]
         syntax/parse
         "../struct.rkt")

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

; Syntax -> String
; converts a piece of syntax to a string
(define (syntax->string stx)
  (local
    (; Syntax -> Boolean
     ; Determines if a Syntax is a constant
     (define (const? exp)
       (not (list? (syntax->datum exp))))
     ; Boolean -> String
     ; converts a boolean to a String
     (define (boolean->string b)
       (if b "#t" "#f"))
     ; Datum -> String
     ; Converts a datum to a String
     (define (stringify exp)
       (cond
         [(symbol? exp) (symbol->string exp)]
         [(number? exp) (number->string exp)]
         [(boolean? exp) (boolean->string exp)]
         [(list? exp) (string-append "(" (string-join (map stringify exp) " ") ")")]
         [else (error "datum cannot be turned to string")])))
    (cond
      [(const? stx) (stringify (syntax->datum stx))]
      [else (string-append
             "(" (string-join (map (compose stringify syntax->datum) (syntax->list stx)) " ")
             ")")])))

; Creates a function that determines if a given DocProp
; matches that type
(define (mk-prop? type)
  (λ (prop) (prop-type=? type (doc-prop-type prop))))

; Returns the first element in the list that matches pred
; Returns an empty list if no such element is found
(define (extract pred lst)
  (cond
    [(empty? lst) lst]
    [else (let ([fst (first lst)])
            (if (pred fst) fst (extract pred (rest lst))))]))