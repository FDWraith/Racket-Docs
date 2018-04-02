#lang racket

(provide type-summary
         type-label
         type-label/union)

(require "struct.rkt"
         "../utils/gen.rkt")

#;(define-docs summary-recur-limit
    [Signature: Nat]
    [Purpose: #<<"
How many layers of sub-types will be used in a type summary or label,
before ... is used. Prevents unnecessary or infinite recursion.
"
              ])
(define summary-recur-limit 10)

#;(define-docs (type-summary type)
    [Signature: Type -> String]
    [Purpose: "A detailed summary of the type, useful for debugging."]
    [Examples:
     (type-summary (λ () (primitive "String"))) => "String"
     (type-summary (labeled-type [Union/parsed (λ () (primitive "Pos"))
                                               (λ () 0)] "Nat")) =>
     "[Union Pos '0]"
     (type-label (labeled-type (λ () (primitive "PosInt")) "Pos")) => "Pos"])
(define (type-summary type)
  (define (type-summary/acc type recurs-left)
    (cond
      [(zero? recurs-left) "..."]
      [else
       (type-summary/recur type
                           (- summary-recur-limit recurs-left)
                           (λ (_) #true)
                           (curryr type-summary/acc (sub1 recurs-left)))]))
  (type-summary/acc type summary-recur-limit))

#;(define-docs (type-label type)
    [Signature: Type -> String]
    [Purpose: "A brief label of the type, useful for displaying to the user."]
    [Examples:
     (type-label (λ () (primitive "String"))) => "String"
     (type-label (labeled-type [Union/parsed (λ () (primitive "Pos"))
                                             (λ () 0)] "Nat")) => "Nat"
     (type-label (labeled-type (λ () (primitive "PosInt")) "Pos")) => "Pos"])
(define (type-label type)
  (define (type-label/acc type recurs-left)
    (cond
      [(labeled-type? type) (labeled-type-label type)]
      [(zero? recurs-left) "..."]
      [else (type-summary/recur type
                                (- summary-recur-limit recurs-left)
                                show-in-intersection-label?
                                (curryr type-label/acc (sub1 recurs-left)))]))
  (type-label/acc type summary-recur-limit))

#;(define-docs (type-label/union type)
    [Signature: Type -> [Listof String]]
    [Purpose: #<<"
A brief label of the type, or possible subtypes of the type if it's a union.
"
              ]
    [Examples:
     (type-label/union (λ () (primitive "String"))) => '("String")
     (type-label/union [Union/parsed (λ () (primitive "String"))
                                    (λ () (primitive "Nat"))]) =>
     '("String" "Nat")
     (type-label (labeled-type (λ () (primitive "PosInt")) "Pos")) => "PosInt"
     (type-label/union
      (labeled-type (labeled-type [Union/parsed (λ () (primitive "Pos"))
                                                (λ () 0)] "Nat") "Natural")) =>
     '("Nat")])
(define (type-label/union type)
  (define type+ (type))
  (cond
    [(union? type+) (map type-label (union-subs type+))]
    [else (list (type-label type))]))

#;(define-docs (show-in-intersection-label? x)
    [Signature: Type -> Boolean]
    [Purpose: #<<"
Whether this type should be shown in a type label,
if it's in an intersection with other types.
"
              ])
(define (show-in-intersection-label? x)
  (define x+ (x))
  (cond
    [(intersection? x+)
     (ormap show-in-intersection-label? (intersection-subs x+))]
    [(union? x+)
     (andmap show-in-intersection-label? (union-subs x+))]
    [(expr? x+) #false]
    [(forall? x+)
     (show-in-intersection-label? ((forall-get-type x+) Nothing/parsed))]
    [else #true]))

#;(define-docs (type-summary/recur type eid sub-filter sub-summary)
    [Signature: Type Nat [Type -> Bool] [Type -> String] -> String]
    [Purpose: #<<"
A summary of the type. Fills in parameterized types with the primitive "X@eid",
ignores intersection sub-types which return false in @sub-filter
(pretends that they don't exist), and summarizes sub-types via @sub-summary.
"
              ]
    [Examples:
     (type-summary/recur (λ () (primitive "String")) (λ (x) "foo")) => "String"
     (type-summary/recur Nat (λ (x) "foo")) => "[Intersection foo foo]"])
(define (type-summary/recur type eid sub-filter sub-summary)
  (define type+ (type))
  (cond
    [(primitive? type+) (primitive-label type+)]
    [(intersection? type+)
     (define all-intersection-subs (intersection-subs type+))
     (define filtered-intersection-subs
       (filter sub-filter (intersection-subs type+)))
     (define intersection-subs*
       (if (empty? filtered-intersection-subs)
           all-intersection-subs
           filtered-intersection-subs))
     (cond
       [(empty? intersection-subs*) "Any"]
       [(empty? (rest intersection-subs*))
        (sub-summary (first intersection-subs*))]
       [else
         (format "[Intersection ~a]"
                 (string-join (map sub-summary intersection-subs*) " "))])]
    [(union? type+)
     (cond
       [(empty? (union-subs type+)) "Nothing/Unknown"]
       [(empty? (rest (union-subs type+)))
        (sub-summary (first (union-subs type+)))]
       [else
        (format "[Union ~a]"
                (string-join (map sub-summary (union-subs type+)) " "))])]
    [(func? type+)
     (format "[~a -> ~a]"
             (string-join (map sub-summary (func-params type+)) " ")
             (sub-summary (func-out type+)))]
    [(forall? type+)
     (define eparam (λ () (primitive (format "X~a" eid))))
     (format "{~a} ~a"
             (sub-summary eparam)
             (sub-summary ((forall-get-type type+) eparam)))]
    [(list? type+)
     (format "(~a)" (string-join (map sub-summary type+) " "))]
    [else (format "~a" type+)]))
