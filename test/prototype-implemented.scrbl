#lang scribble/manual

@require[scribble/example]

@title{Prototype-Implemented Documentation}
@secref{datadefs}

@secref{constants}

@secref{functions}

@secref{macros}

@section[#:tag "datadefs"]{Data Definitions}
@defthing[#:kind "data defintion" #:link-target? #f OpChain Type #:value (code:line (cons Num '())
(cons Num (cons Operator OpChain)))]{
A combination of infix operators and numbers, without parenthesis.
}

@bold{Examples:}
@racketblock[(cons 4 '()) (code:comment "4")]

@racketblock[(cons 1 (cons '+ (cons 2 (cons '* (cons 3 '()))))) (code:comment "1 + 2 * 3")]

@racketblock[(cons -3.5 (cons '/ (cons 1/2 '()))) (code:comment "-3.5 / 1/2")]



@defthing[#:kind "data defintion" #:link-target? #f Operator Type #:value (code:line '+
'-
'*
'/)]{
A mathematical operator, describes how 2 numbers are combined.
}



@defthing[#:kind "data defintion" #:link-target? #f Natural Type #:value (code:line 0
(add1 Natural))]{
A natural number.
}

@bold{Examples:}
@racketblock[0]

@racketblock[(add1 (add1 (add1 0))) (code:comment "3")]



@defthing[#:kind "data defintion" #:link-target? #f (Tree X) Type #:value (code:line (tree1.0 X [Listof [Tree X]]))]{
A tree.
@racket[value] refers to the top node.
@racket[children] refers to child trees. Each @racket[value] in @racket[children] is a child node,
each @racket[value] in @racket[children] of @racket[children] is a grandchild node, and so on.
}

@bold{Examples:}
@racketblock[(tree 12 '())]

@racketblock[(tree "A" (list (tree "B" '()) (tree "C" (list (tree "D" '())))))]

@racketblock[(tree (tree "Complex" '()) (list (tree "Tree" '())))]



@section[#:tag "constants"]{Constants}

@section[#:tag "functions"]{Functions}
@defproc[#:link-target? #f (string* [str (code:line String)] [n (code:line Nat)]) (code:line String)]{
Appends the given @racket[str] to itself @racket[n] times.

}

@bold{Examples:}
@examples[#:label #false (eval:alts (string* "ABC" 0) (eval:result (racket "") "" ""))]

@examples[#:label #false (eval:alts (string* "ABC" 1) (eval:result (racket "ABC") "" ""))]

@examples[#:label #false (eval:alts (string* "Foo!" 3) (eval:result (racket "Foo!Foo!Foo!") "" ""))]

@examples[#:label #false (eval:alts (string* "Hello" 4) (eval:result (racket "HelloHelloHelloHello") "" ""))]



@defproc[#:link-target? #f (mk-tree [value (code:line X0)] [children (code:line [Listof [Tree X0]])]) (code:line [Tree X0])]{
Creates a tree.

}



@defproc[#:link-target? #f (annotate-depth [t0 (code:line [Tree X0])]) (code:line [Tree (cons X0 (cons Nat '()))])]{
Pairs each element in the @racket[tree] with its level, where the root of the tree
has level 0.

}

@bold{Examples:}
@examples[#:label #false (eval:alts (annotate-depth (tree 'x '())) (eval:result (racket (tree (list 'x 0) '())) "" ""))]

@examples[#:label #false (eval:alts (annotate-depth (tree "Hello" (list (tree "World" '()) (tree "!" '())))) (eval:result (racket (tree (list "Hello" 0) (list (tree (list "World" 1) '()) (tree (list "!" 1) '())))) "" ""))]

@examples[#:label #false (eval:alts (annotate-depth (tree 3 (list (tree 5 (list (tree 7 '())))))) (eval:result (racket (tree (list 3 0) (list (tree (list 5 1) (list (tree (list 7 2) '())))))) "" ""))]

@examples[#:label #false (eval:alts (annotate-depth (tree (list 3 0) (list (tree (list 5 1) (list (tree (list 7 2) '())))))) (eval:result (racket (tree (list (list 3 0) 0) (list (tree (list (list 5 1) 1) (list (tree (list (list 7 2) 2) '())))))) "" ""))]



@section[#:tag "macros"]{Macros}
