# Language and Grammar

The language and grammar is identical to Racket, except `Expr` has more
potential forms:

```text
Expr = ... ; From Racket
     | (define-docs Head
         [Signature: Type]
         [Purpose: RawText]
         ExtraDoc ...)
     | (define-data Id
         [: UnionType]
         [Interpretation: RawText]
         ExtraDoc ...)

Head = Id
     | (Id Id ...)

ExtraDoc = [Examples: Example ...]
         | [Accumulator: Id : RawText]
         | [Generative: RawText]

UnionType = Type
          | TypeOption TypeOption ...
          | {Id ...} Type
          | {Id ...} TypeOption TypeOption ...

TypeOption = - Type

Type = Id
     | (Id Type ...)
     | [Type Type ...]
     | Type ... -> Type

Example = Expr
        | Expr => Expr
        | RawText <= Expr

#|
RawText doesn't follow traditional Racket syntax - it's similar to
Scribble syntax. Code is spliced in with either @expr or @expr@. In the
former, the code ends at the next unenclosed space, and in the latter
the code ends at the next unenclosed @ (examples: @foo bar -> @foo,
@foo@bar -> @foo, @(a b c) d -> @(a b c), @(a @ b)@ c -> @(a @ b)). @@
prevents code from being spliced (e.g. @@foo or @@foo@@bar wouldn't
splice any code).
#|
```

Note that parenthesis aren't checked (e.g. `(Purpose: ...)` is fine).
Everything is parsed according to Racket, except raw text, which is
parsed like Scribble but with slightly different @ semantics.

---

In our initial implementation, to make sure we have time to implement
the "main" parts, we'll use a slightly different data definition. It's
the same as the definition above, except:

```

RawText = String
; For multiline text, can use Racket's multiline quote syntax - #<<"
; ...
; "

UnionType = Type
          | TypeOption TypeOption ...
          ; No {Id ...}
```
