#lang scribble/manual

@title{Racket Docs}

@section{Data Definitions}
@defthing[ #:kind "Data Defintion" String/False String/False? #:value (code:line String
Bool)
]{
A string or false.
}


@section{Constant Definitions}
@defthing[ #:kind "Constant"hello String/False]{
Hello}


@section{Function Definitions}
@defproc[(+int [x Int] [y Int]) Int ]{
Adds 2 integers.}

@defproc[(+list [x [Listof String]] [y [Listof String]]) [Listof String] ]{
Combines 2 lists}

@defproc[(+list-5 [x [Listof String]] [y ???]) [Listof String] ]{
Combines 2 lists}

