#lang scribble/manual

@title{Racket Docs}

@section{Data Definitions}
@defthing[#:kind "Data Defintion" String/False String/False? #:value (code:line String Bool)
]{
A string or false.
}

@section{Constant Definitions}
@defproc[(+int [x Int] [y Int]) Int]{
Adds 2 integers.
}

@defproc[(+list [x [Listof String]] [y [Listof String]]) [Listof String]]{
Combines 2 lists
}

@defproc[(+tuple [x (#<procedure:.../types/parse.rkt:33:15> String (#<procedure:.../types/parse.rkt:33:15> Int (#<procedure:.../types/parse.rkt:33:15> Nat '())))] [y (#<procedure:.../types/parse.rkt:33:15> String (#<procedure:.../types/parse.rkt:33:15> Int (#<procedure:.../types/parse.rkt:33:15> Nat '())))]) (#<procedure:.../types/parse.rkt:33:15> String (#<procedure:.../types/parse.rkt:33:15> Int (#<procedure:.../types/parse.rkt:33:15> Nat '())))]{
Combines 2 tuples
}

@defproc[(+list-5 [x [Listof String]] [y [Listof 5]]) [Listof String]]{
Combines 2 lists
}

@section{Function Definitions}
@defthing[#:kind "Constant" hello String/False]{
Hello
}

@section{Macro Definitions}
