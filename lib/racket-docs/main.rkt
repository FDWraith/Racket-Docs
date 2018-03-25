#lang racket

(provide [except-out [all-from-out racket]
                     read
                     read-syntax
                     read-language
                     #%datum
                     #%app]
         [rename-out (typed-datum #%datum)
                     (typed-app #%app)]
         
         [for-syntax define-docs
                     define-data
                     define-syntax/docs
                     compile-docs
                     no-docs?]
         define-docs
         define-data
         define-syntax/docs
         begin-without-type-checking
         
         [all-from-out "types/builtin.rkt"])

(require [for-syntax "compile.rkt"]
         "parse.rkt"
         "types/language.rkt"
         "types/builtin.rkt")

