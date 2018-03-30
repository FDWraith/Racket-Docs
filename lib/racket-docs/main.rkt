#lang racket

(provide [except-out [all-from-out racket]
                     read
                     read-syntax
                     read-language
                     #%module-begin
                     #%datum
                     #%app
                     list]
         [rename-out (typed-module-begin #%module-begin)
                     (typed-datum #%datum)
                     (typed-app #%app)]
         
         [for-syntax define-docs
                     define-data
                     define-syntax/docs
                     compile-docs
                     get-all-docs
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

