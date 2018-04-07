#lang racket

(provide stxparse-option
         pattern-option
         temp-matches
         mk-id-macro
         mk-applied-id-macro
         map/stx
         flatten/stx
         stx-expression?
         add-bindings
         syntax-property/recur
         identifier->stx-string
         syntax->string
         extract)

(require [for-syntax syntax/parse]
         syntax/parse
         "../types/macrotypes/stx-utils.rkt")

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

; {X X2... Y} [X X2... -> Y] [Stx [Listof X]] [Listof X2]... -> [Stx [Listof Y]]
; Transforms the list within the syntax.
(define (map/stx f stx . xs)
  (datum->syntax stx (apply map f (syntax->list stx) xs)))

; {X} [Stx [Listof [Listof X]]] -> [Stx [Listof X]]
; Flattens the list within the syntax.
(define (flatten/stx stx)
  (datum->syntax stx (foldr append '()
                            (map syntax-e
                                 (syntax-e stx)))))

; [Stx [Listof Identifier]] [Stx [Listof Syntax]] -> [Stx [Listof Syntax]]
; Resolves unbound identifiers in stx using the identifiers from bindings-stx.
(define (add-bindings bindings-stx stx)
  (define ctx (syntax-local-make-definition-context))
  (syntax-local-bind-syntaxes (syntax->list bindings-stx) #f ctx)
  (local-expand stx 'expression (list #'#%datum #'#%app)))

; Syntax Symbol Any -> Syntax
; Sets the syntax property in the given syntax and any of its children
; (e.g. if it's a syntax list, the syntaxes inside the list).
(define (syntax-property/recur stx key val)
  (define (syntax-property/recur* stx*)
    (cond
      [(cons? stx*)
       (cons (syntax-property/recur* (car stx*))
             (syntax-property/recur* (cdr stx*)))]
      [(empty? stx*) '()]
      [else (syntax-property/recur stx* key val)]))
  (define stx+ (syntax-property stx key val))
  (define stx+e (syntax-e stx+))
  (cond
    [(cons? stx+e)
     (datum->syntax stx+
                    (cons (syntax-property/recur* (car stx+e))
                          (syntax-property/recur* (cdr stx+e))))]
    [else stx+]))

; Syntax -> Bool
; Whether the syntax is an expression, not a definition.
(define stx-expression?
  (syntax-parser
    [((~datum define) x ...) #false]
    [((~datum define-syntax) x ...) #false]
    [((~datum define-values) x ...) #false]
    [((~datum define-syntaxes) x ...) #false]
    [((~datum define-docs) x ...) #false]
    [_ #true]))

; Identifier -> [Stx String]
; Converts an identifier to a string of its name.
; Example: (identifier->stx-string #'foo) => #'"foo"
(define (identifier->stx-string id)
  (datum->syntax id (symbol->string (syntax-e id))))

; Syntax -> String
; Converts a piece of syntax to a string.
(define (syntax->string stx)
  (datum->string (syntax->datum stx)))

; Any -> String
; Converts the datum to a string which would be printed with display.
; (Copied from gen.rkt, because importing gave phase linking errors).
(define (datum->string x)
  (cond
    [(cons? x)
     (if (equal? (first x) 'quote)
         (format "'~a" (datum->string (second x)))
         (string-join (map datum->string x) " "
                      #:before-first "("
                      #:after-last ")"))]
    [(string? x) (format "~v" x)]
    [else (format "~a" x)]))

; Returns the first element in the list that matches pred
; Returns an empty list if no such element is found
(define (extract pred lst)
  (cond
    [(empty? lst) lst]
    [else (let ([fst (first lst)])
            (if (pred fst) fst (extract pred (rest lst))))]))