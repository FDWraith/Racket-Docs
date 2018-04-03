#lang scribble/manual

@require[scribble/example]

@title{Racket-Docs-Code Documentation}
@secref{datadefs}
@secref{constants}
@secref{functions}
@secref{macros}

@section[#:tag "datadefs"]{Data Definitions}

@section[#:tag "constants"]{Constants}
@defthing[#:kind "constant" #:link-target? #f VOWELS (code:line [Listof Char])]{
All the vowels in the alphabet, in lowercase.
}



@section[#:tag "functions"]{Functions}
@defproc[#:link-target? #f (shorthand [str (code:line String)]) (code:line String)]{
Removes vowels from @racket[str.]

}

@bold{Examples:}
@examples[#:label #false (eval:alts (shorthand "") (eval:result (racket "") "" ""))]

@examples[#:label #false (eval:alts (shorthand "world") (eval:result (racket "wrld") "" ""))]

@examples[#:label #false (eval:alts (shorthand "ApPle;") (eval:result (racket "pPl;") "" ""))]



@section[#:tag "macros"]{Macros}
