#lang racket

(provide [except-out [all-from-out racket]
                     read
                     read-syntax
                     read-language]
         [for-syntax define-docs
                     define-data
                     define-syntax/docs
                     compile-docs]
         define-docs
         define-data
         define-syntax/docs
         compile-docs
         get-all-docs
         
         ; Special types
         Union
         ->
         ; Primitive Types
         Bool
         Pos
         Nat
         Int
         Num
         String

         ; Built-In Non-Primitive Types
         Listof
         Maybe)

(require [for-syntax "compile.rkt"]
         "parse.rkt"
         "types.rkt")