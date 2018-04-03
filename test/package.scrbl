#lang scribble/manual

@require[scribble/example]

@title{Package Documentation}
@secref{datadefs}

@secref{constants}

@secref{functions}

@secref{macros}

@section[#:tag "datadefs"]{Data Definitions}
@defthing[#:kind "data defintion" #:link-target? #f StxDocumentedInt Type #:value (code:line Nat
NegInt)]{
A data definition created in phase 1.
}

@bold{Examples:}
@racketblock[5]



@defthing[#:kind "data defintion" #:link-target? #f (Either X Y) Type #:value (code:line X
Y)]{
A data definition constructor.
}



@defthing[#:kind "data defintion" #:link-target? #f String/False Type #:value (code:line String
Bool)]{
A string or false.
}

@bold{Examples:}
@racketblock["Hello"]

@racketblock["World"]

@racketblock[#f]



@section[#:tag "constants"]{Constants}
@defthing[#:kind "constant" #:link-target? #f hello (code:line String/False)]{
Hello
}

@bold{Examples:}
@examples[#:label #false (eval:alts hello (eval:result (racket "Hello") "" ""))]






@defthing[#:kind "constant" #:link-target? #f phase1-val (code:line Int)]{
A value which could be used by macros.
}

@bold{Examples:}
@examples[#:label #false (eval:alts phase1-val (eval:result (racket 5) "" ""))]






@section[#:tag "functions"]{Functions}
@defproc[#:link-target? #f (+int [x (code:line StxDocumentedInt)] [y (code:line Int)]) (code:line Int)]{
Adds 2 integers.

}


@bold{Accumulator} - @racket[x]:
Not actually an accumulator.

@bold{Effects:}
No effects

@defproc[#:link-target? #f (+list [x (code:line [Listof String])] [y (code:line [Listof String])]) (code:line [Listof String])]{
Combines 2 lists

}






@defproc[#:link-target? #f (+tuple [x (code:line (cons String (cons Int (cons Nat '()))))] [y (code:line (cons String (cons Int (cons Nat '()))))]) (code:line (cons String (cons Int (cons Nat '()))))]{
Combines 2 tuples

}






@defproc[#:link-target? #f (+list-5 [x (code:line [Listof String])] [y (code:line [Listof 5])]) (code:line [Listof String])]{
Combines 2 lists

}



@bold{Generative:}
This isn't generative.


@section[#:tag "macros"]{Macros}
@defform[#:link-target? #f #:id provide ((provide identifier ...)) ]{
Exports @racket[identifier], so other @racket[module]s can @racket[require] it.
}





