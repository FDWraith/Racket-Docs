#lang racket

(provide map/unzip
         map/maybe
         list->values
         values->list)

(require [for-syntax syntax/parse])

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