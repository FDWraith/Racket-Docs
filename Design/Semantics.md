# Semantics

A Racket+Docs program has the same runtime semantics as a Racket
programs, except that it has  an additional function,
`(compile-docs path)`. At compile time, types are checked, and this can
generate errors. When tested, examples are verified, and those can also
generate errors.

When the program is compiled, there's a phase-1 global variable with the
documentation, Each `(define-docs ...)` and `(define-data ...)` adds a
section of documentation to this variable via `set!`. The global
variable is then redefined in phase-0, inside of the function
`(compile-docs path)`. This function takes the documentation, then
compiles it into Scribble, then compiles the Scribble into a collection
of webpages, and finally writes these webpages to the directory at
`path`.

Racket+Docs has types. These are implemented using turnstile. Whenever
a value is documented, if the value is defined in a scope which is
accessible from the documentation's scope, the value will be redefined
to have the type specified in `[Signature: ...]`. Whenever a function is
documented, it will additionally be redefined to check that its inputs
satisfy its contract at compile time. Whenever a data definiton is
documented, it will define a type which has the same identifier as the
definition, and which matches each of its potential types specified in
`[: ...]`.

When Racket+Docs programs are tested (in submodule `test`), all examples
in `define-docs` are verified using rackunit. Each example has the form
`x => y`, where `x` and `y` are values. When verified,
`(check-equal? x y)` is run.
