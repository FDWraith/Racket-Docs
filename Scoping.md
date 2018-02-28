# Scoping Rules

The language is scoped the same way as Racket, except for custom forms.

-   In general, code in a `define-docs` like
    ```r
    (define-docs (foo x)
      [: Any -> Any]
      [Purpose: @foo using @x.]
      x)
    ```
    has the same scope as the definition itself, except it can refer to
    the value being defined and its arguments. In the example, `@foo`
    refers to `foo` itself, and `@x` refers to the argument `x`.

    An exception is code in examples or types. This code has the same
    scope as the definition itself, except it can refer to the value
    being defined, but if the *can't* refer to the value's arguments
    (if the value has arguments). For example, in
    ```r
    (define (foo x)
      [: Any -> Any]
      [Purpose: ...]
      [Examples:
       x => "bar"]
      x)
    ```
    the example's `x` *wouldn't* refer to the argument in `foo`.

-   Code spliced in raw text via `@` in a `define-data` like
    ```r
    (define-data Foo
      [: Int]
      [Interpretation: @Foo])
    ```
    has the same scope as the definition itself, except it can refer to
    the type being defined (like with `define-docs`). In the example,
    `@foo` example, `Foo` refers to the definition `Foo` itself.

In the above rules, "code" refers to
- Types in declarations - `[: ...]` and `[Signature: ...]`.
- Code spliced in raw text via `@`.
- Code in examples.
- The identifier `x` in `[Accumulator: x : ...]`.

Types in declarations bind. In `(define-data X [: Y ...] ...)`, the type
`X` will be created, and it will be equivalent to type `Y ...`. In
`(define-docs x [Signature: Y ...] ...)` or
`(define-docs (f x ...) [Signature: Y ...]) ...`, the value `x` or `f`
will have type `Y ...`. As mentioned above, these types bind in the same
scope as the definition itself.

Type variables are declared inside `{...}` in a type  declaration, e.g.
`[: {A B C} X -> Y]` declares the type variables `A`, `B`, and `C`. Not
every type declaration contains a `{...}`, in which case there are no
type variables inside the type declaration. These declared type
variables are local - they bind within the type declaration, but not
outside of it. So e.g. `[: {A B C} A -> B]` is valid, because `A` and
 `B` in `A -> B` are bound, but `[: {A B C} D -> E]` don't, because `D`
and `E` are unbound.

Types are all uppercase. Lowercase identifiers are allowed, in which
case they're treated as literal expressions. For example
`(list Num Num)` is a valid type. Like with types or other expressions,
identifiers in these literal expressions bind in the same scope as the
definition itself.
