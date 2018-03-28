#lang racket

(provide type-error-msg
         types-error-msg-of
         app-error-msg
         types-error-idxs
         app-error-param-idxs
         get-type-error
         get-types-error
         get-app-error)

(require "subtype.rkt"
         "struct.rkt"
         "../utils.rkt")

#;(define-data TypeError
    [: (bad-type Type Type)]
    [Interpretation: "Created when the type given is different than expected."]
    [Examples:
     "Expected String, got Int" <= (type-error String Int)])
(struct bad-type [expected actual])

#;(define-data TypesError
    [: - (bad-length Nat Nat)
       - (bad-type-at-idx Nat TypeError)]
    [Interpretation: "Created when types given are different than expected."]
    [Examples:
     "Expected 5 types, got 7" <= (bad-length 5 7)
     "3rd type: Expected String, got Nat" <=
     (bad-type-at-idx 3 (type-error String Nat))])
(struct bad-length [expected actual])
(struct bad-type-at-idx [idx err])

#;(define-data AppError
    [: - (not-func Type)
       - (bad-params TypesError)
       - (no-overloads [Listof OverloadError])]
    [Interpretation: "Created from an invalid function application."]
    [Examples:
     "Expected a function, got Any" <= (not-func Any)
     "2nd param: Expected '0, got NegInt" <=
     (bad-params (bad-type-at-idx 2 (bad-type (λ () 0) NegInt)))
     #<<"
None of the function's overloads could accept the parameters:
- [Int String -> Void]: Expected 2 params, got 1
- [Char String -> Void]: Expected 2 params, got 1
"
     <= (no-overloads (list (list [-> Int String Void] (bad-length 2 1))
                            (list [-> Char String Void] (bad-length 2 1))))])
(struct not-func [type])
(struct bad-params [err])
(struct no-overloads [sub-errs])

#;(define-data OverloadError
    [: (bad-overload Type TypesError)]
    [Interpretation:
     "Created from an invalid overload in a function application."])
(struct bad-overload [type err])

#;(define-docs (type-error-msg err)
    [Signature: TypeError -> String]
    [Purpose: "Describes the type error."]
    [Examples:
     (type-error-msg (type-error String Int)) => "Expected String, got Int"])
(define (type-error-msg err)
  (format "Expected ~a, got ~a"
          (type-label (bad-type-expected err))
          (type-label (bad-type-actual err))))

#;(define-docs (types-error-msg-of err [type "type"])
    [Signature: [Intersection [TypesError String -> String]
                              [TypesError -> String]]]
    [Purpose: "Describes the types error, where @type describes the type."]
    [Examples:
     (types-error-msg-of (bad-length 5 7) "type") => "Expected 5 types, got 7"
     (types-error-msg-of (bad-type-at-idx 3 (type-error String Nat)) "foo") =>
     "3rd foo: Expected String, got Nat"])
(define (types-error-msg-of err [type "type"])
  (match err
    [(bad-length expected actual)
     (format "Expected ~a ~as, got ~a" expected type actual)]
    [(bad-type-at-idx idx err*)
     (format "~a ~a: ~a" (idx->ordinal idx) type (type-error-msg err*))]))

#;(define-docs (app-error-msg err)
    [Signature: AppError -> String]
    [Purpose: "Describes the function application error."]
    [Examples:
     (app-error (not-func Any)) => "Expected a function, got Any"
     (app-error (bad-params (bad-type-at-idx 2 (bad-type (λ () 0) NegInt)))) =>
     "2nd param: Expected '0, got NegInt"
     (app-error (no-overloads `((,[-> Int String Void] ,(bad-length 2 1))
                                (,[-> Char String Void] ,(bad-length 2 1))))) =>
     #<<"
None of the function's overloads could accept the parameters:
- [Int String -> Void]: Expected 2 params, got 1
- [Char String -> Void]: Expected 2 params, got 1
"
     ])
(define (app-error-msg err)
  (match err
    [(not-func type)
     (format "Expected a function, got ~a" (type-label type))]
    [(bad-params err*) (types-error-msg-of err* "param")]
    [(no-overloads sub-errs)
     (if (empty? (rest sub-errs))
         (types-error-msg-of (bad-overload-err (first sub-errs)) "param")
         (string-join
          (cons "None of the function's overloads could accept the parameters:"
                (map bad-overload-error-msg sub-errs))
          "\n"))]))

#;(define-docs (bad-overload-error-msg err)
    [Signature: OverloadError -> String]
    [Purpose: "Describes the function application overload error."])
(define (bad-overload-error-msg err)
  (format "- ~a: ~a"
          (type-label (bad-overload-type err))
          (types-error-msg-of (bad-overload-err err) "param")))

#;(define-docs (types-error-idxs err)
    [Signature: TypesError -> [Listof Nat]]
    [Purpose: "Gets the indices of any types blamed by the types error."])
(define (types-error-idxs err)
  (cond
    [(bad-type-at-idx? err) (list (bad-type-at-idx-idx err))]
    [else '()]))

#;(define-docs (app-error-param-idxs err)
    [Signature: AppError -> [Listof Nat]]
    [Purpose:
     "Gets the indices of any parameters blamed by the application error."])
(define (app-error-param-idxs err)
  (match err
    [(not-func _) '()]
    [(bad-params err*) (types-error-idxs err*)]
    [(no-overloads sub-errs)
     (append-map (compose types-error-idxs bad-overload-err) sub-errs)]))

#;(define-docs (get-type-error x y)
    [Signature: Type Type -> [Maybe TypeError]]
    [Purpose: #<<"
If @x isn't a subtype of @y, returns an error indicating this.
"
              ]
    [Examples:
     (get-type-error Int String) => (type-error String Int)
     (get-type-error Int Num) => #false
     (get-type-error Any Int) => (type-error Int Any)])
(define (get-type-error x y)
  (and (not (type<? x y))
       (bad-type y x)))

#;(define-docs (get-types-error xs ys)
    [Signature: [Listof Type] [Listof Type] -> [Maybe TypesError]]
    [Purpose: #<<"
If there aren't equal amounts of @xs and @ys, or
of an element in @xs isn't a subtype of the element in @ys with the same index,
returns an error indicating this.
"
              ])
(define (get-types-error xs ys)
  (cond
    [(not (equal? (length xs) (length ys)))
     (bad-length (length ys) (length xs))]
    [else (ormap get-type-at-idx-error xs ys (range (length xs)))]))

#;(define-docs (get-type-at-idx-error xs ys idx)
    [Signature: Type Type Nat -> [Maybe TypesError]]
    [Purpose: #<<"
If @x isn't a subtype of @y, returns an error indicating this,
in the context where @x and @y are both at @idx in of a list of types.
"
              ])
(define (get-type-at-idx-error xs ys idx)
  (map/maybe (λ (err) (bad-type-at-idx idx err))
             (get-type-error xs ys)))

#;(define-docs (get-app-error xs f)
    [Signature: [Listof Type] Type -> [Maybe AppError]]
    [Purpose: #<<"
If @f isn't a function or @xs aren't subtypes of @f's parameters,
returns an error indicating this.
"
              ])
(define (get-app-error xs f)
  (define f+ (f))
  (cond
    [(intersection? f+)
     (get-rough-overload-error f (intersection-subs f+) xs)]
    [(union? f+)
     (ormap (curry get-app-error xs) (union-subs f+))]
    [(func? f+) (get-params-error xs (func-params f+))]
    [else (not-func f)]))

#;(define-docs (get-rough-overload-error fs xs)
    [Signature: [Listof Type] [Listof Type] -> [Maybe AppError]]
    [Purpose: #<<"
If no subtypes of @f (@fs) can be applied with @xs,
returns an error indicating this.
"
              ])
(define (get-rough-overload-error f fs xs)
  (define (try-rough-overload-error fs errs)
    (cond
      [(findf false? errs) #false]
      [(findf no-overloads? errs)
       (match-define-values
         [(list (list pre-intersection pre-no-overloads) ...)
          (list (list intersection no-overloads)
                (list post-intersection post-no-overloads) ...)]
         (splitf-at (map list fs errs) (compose no-overloads? second)))
       (match-define (list (bad-overload intersection-subs overload-errs) ...)
         (no-overloads-sub-errs no-overloads))
       (try-rough-overload-error
        (append pre-intersection intersection-subs post-intersection)
        (append pre-no-overloads overload-errs post-no-overloads))]
      [(findf bad-params? errs)
       (no-overloads (map (curry apply bad-overload)
                          (map (curryr list-update 1 bad-params-err)
                               (filter (compose bad-params? second)
                                       (map list fs errs)))))]
      [else (not-func f)])) ; All errors are not-func, this isn't a function
  (try-rough-overload-error fs (map (curry get-app-error xs) fs)))

#;(define-docs (get-params-error xs ys)
    [Signature: [Listof Type] [Listof Type] -> [Maybe AppError]]
    [Purpose: #<<"
If there aren't equal amounts of @xs and @ys, or
of an element in @xs isn't a subtype of the element in @ys with the same index,
returns an error indicating this,
in a context where @xs and @ys are actual and expected params, respectively.
"
              ])
(define (get-params-error xs ys)
  (map/maybe bad-params (get-types-error xs ys)))