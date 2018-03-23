#lang racket

(provide [except-out [all-from-out racket]
                     read
                     read-syntax
                     read-language]
         [for-syntax define-docs
                     define-data
                     define-syntax/docs
                     get-all-docs
                     compile-docs]
         define-docs
         define-data
         define-syntax/docs
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

(require "parse.rkt")
(require "types.rkt")
(require (for-syntax "compile.rkt"))