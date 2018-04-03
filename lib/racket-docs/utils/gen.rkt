#lang racket

(provide equal-thunk-limit
         map/unzip
         map/maybe
         transpose
         pad-right
         list->values
         values->list
         thunk?
         equal-datum?
         datum->string
         idx->ordinal
         cardinal->ordinal)

(require [for-syntax syntax/parse])

; How many thunks are evaluated in equal-datum before they're results are just
; considered equal.
(define equal-thunk-limit 100)

(define (map/unzip f n . xs)
  (define (add-values new-values values-list)
    (unless (equal? (length new-values) (length values-list))
      (error "Expected ~a values, got ~a"
             (length values-list)
             (length new-values)))
    (map cons new-values values-list))
  (define (trans/add-values . xs)
    (match-define-values (new-values (list values-list)) (split-at-right xs 1))
    (add-values (values->list (apply f new-values)) values-list))
  (define init-values (build-list n (λ (_) '())))
  (list->values (apply foldr trans/add-values init-values xs)))

; [X -> Y] [Maybe X] -> [Maybe Y]
; Only transforms if not #false.
(define (map/maybe f x)
  (cond
    [x (f x)]
    [else x]))

; [Listof List] -> [Listof List]
; Rearranges rows and columns in a 2D list.
(define (transpose xs)
  (cond
    [(empty? xs) '()]
    [(cons? xs) (apply map list xs)]))

; [Listof X] X Nat -> [Listof X]
; If the length of the list is smaller than the given length,
; appends the given item until it's equal to the given length.
(define (pad-right xs x target)
  (cond
    [(>= (length xs) target) xs]
    [else (append xs (build-list (- target (length xs)) (λ (y) x)))]))

(define (list->values xs)
  (apply values xs))

(define-syntax values->list
  (syntax-parser
    [(_ x) #'(call-with-values (λ () x) list)]))

; Any -> Bool
; Whether this is a 0-argument procedure.
(define (thunk? x)
  (and (procedure? x) (arity-includes? 0 (procedure-arity x))))

; Any Any -> Bool
; Whether the given values are "equal" in a looser sense then equal?.
; If both values are syntax objects,
; their syntax information is strict and only their datum values are compared.
; If both values are thunks (procedures which take 0 arguments),
; they're evaluated and their results are compared -
; after equal-thunk-limit thunks are evaluated,
; comparisons between thunks will just return #true.
; Otherwise the values are checked with equal/recur?,
; checking sub-values with equal-datum?.
(define (equal-datum? x y)
  (define (equal-datum?/acc x y thunks-left)
    (define (equal-datum?* x y)
      (equal-datum?/acc x y thunks-left))
    (cond
      [(and (syntax? x) (syntax? y))
       (equal-datum?* (syntax->datum x) (syntax->datum y))]
      [(and (thunk? x) (thunk? y))
       (or (zero? thunks-left)
           (equal-datum?/acc (x) (y) (- thunks-left 1)))]
      [else (equal?/recur x y equal-datum?*)]))
  (equal-datum?/acc x y equal-thunk-limit))

; Any -> String
; Converts the datum to a string which would be printed with display.
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

; Nat -> String
; Converts an index into an ordinal.
; Examples: 0 -> 1st, 1 -> 2nd, 2 -> 3rd, ...
(define (idx->ordinal idx)
  (cardinal->ordinal (add1 idx)))

; Pos -> String
; Converts a cardinal into an ordinal.
; Examples: 1 -> 1st, 2 -> 2nd, 3 -> 3rd, ...
(define (cardinal->ordinal card)
  (format "~a~a" card (ordinal-suffix card)))

; Pos -> String
; The suffix for the ordinal corresponding to the cardinal.
; Examples: 1 -> 1st, 2 -> 2nd, 3 -> 3rd, ...
(define (ordinal-suffix card)
  (cond
    [(> card 100) (ordinal-suffix (- card 100))]
    [(> card 20) (ordinal-suffix (- card 20))]
    [(= card 1) "st"]
    [(= card 2) "nd"]
    [(= card 3) "rd"]
    [else "th"]))