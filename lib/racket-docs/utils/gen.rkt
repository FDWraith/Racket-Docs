#lang racket

(provide equal-thunk-limit
         map/unzip
         map/maybe
         list->values
         values->list
         thunk?
         equal-datum?)

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

(define (map/maybe f x)
  (cond
    [x (f x)]
    [else x]))

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