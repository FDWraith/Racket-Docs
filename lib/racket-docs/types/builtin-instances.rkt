#lang racket

(require syntax/parse
         [for-template "use.rkt"
                       "builtin.rkt"
                       racket
                       [prefix-in un: racket]])

(provide add-builtin-instances)

(define (add-builtin-instances src-stx)
  (with-syntax [(.... (quote-syntax ...))]
    (define (local datum)
      (datum->syntax src-stx
                     datum
                     src-stx))
    #`(begin
        (assign-type/id #,(local '+)
          [Intersection [-> Nat Nat Nat]
                        [-> Int Int Int]
                        [-> PosInt PosInt PosInt]
                        [-> NegInt NegInt NegInt]
                        [-> Num Num Num]])
        (assign-type/id #,(local '-)
          [Intersection [-> Nat Nat Nat] ; Not always true - weak typing
                        [-> Int Int Int]
                        ; Not always trye - weak typing
                        [-> PosInt PosInt PosInt]
                        ; Not always true - weak typing
                        [-> NegInt NegInt NegInt]
                        [-> Num Num Num]])
        (assign-type/id #,(local '*)
          [Intersection [-> Nat Nat Nat]
                        [-> Int Int Int]
                        [-> PosInt PosInt PosInt]
                        [-> NegInt NegInt NegInt]
                        [-> Num Num Num]])
        (assign-type/id #,(local '/)
          [Intersection [-> Nat Nat Nat] ; Not always true - weak typing
                        [-> Int Int Int] ; Not always true - weak typing
                        [-> Num Num Num]])

        (assign-type/id #,(local 'cons)
          [Forall X [-> X [Listof X] [Listof X]]])

        (assign-type/id #,(local 'append)
          [Forall X [-> [Listof X] [Listof X] [Listof X]]])

        (define-syntax #,(local 'list)
          (syntax-parser
            [(_) #''()]
            [(_ x xs ....)
             (datum->syntax this-syntax
                            (list (datum->syntax this-syntax 'cons)
                                  #'x
                                  #'(#,(local 'list) xs ....)))])))))