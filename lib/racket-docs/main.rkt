#lang racket

(provide [except-out [all-from-out racket]
                     read
                     read-syntax
                     read-language
                     #%top
                     #%datum
                     #%app
                     define
                     #;[all-from-out "types/builtin-instances.rkt"]]
         [rename-out (typed-top #%top)
                     (typed-datum #%datum)
                     (typed-app #%app)
                     (typed-define define)]
         
         [for-syntax define-docs
                     define-data
                     define-syntax/docs
                     compile-docs
                     no-docs?]
         define-docs
         define-data
         define-syntax/docs
         begin-without-type-checking
         [all-from-out "types/builtin-instances.rkt"]
         [all-from-out "types/builtin.rkt"])

(require [for-syntax "compile.rkt"]
         "parse.rkt"
         "types/language.rkt"
         "types/builtin-instances.rkt"
         "types/builtin.rkt")

