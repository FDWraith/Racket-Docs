# Racket-Docs

## Purpose

*Racket-Docs* is a documentation language for Racket. Its primary purpose is to allow users to document code at the same time that they are writing it. After the programmer is done typing, the documentation can be compiled into `HTML` at the press of a button. In addition, *Racket-Docs* also features type-checking and examples-checking in its documentation forms, so that the programmer can ensure that his/her code match up to what he/she designed.

## Vocabulary and Grammar

*Racket-Docs* introduces two new forms, `define-data` and `define-docs`, which are used to define data-definitions and function documentation.

```text
#lang racket-docs

racket-doc-expr = racket-expr ; Expressions From Racket
				| doc-expr 
				
doc-expr = (define-docs head-expr
		 	[Signature: type-expr]
			[Purpose: String]
			extra-doc-expr ...) ; No repeats of same type of doc
	     | (define-docs id
	        [Syntax: racket-expr]
	        [Semantics: String]
	        extra-doc-expr ...) ; No repeats of same type of doc
	     | (define-data id
	        [: - type-expr
	           ...+] 
	        [Interpretation: String]
	        data-extra-expr ...) ; No repeats of same type of doc
	        
head-expr = id
		  | (id id ...+)

data-extra-expr = [Examples: example-expr ...+]

extra-doc-expr = [Examples: example-expr ...+]
			   | [Accumulator: Id: String]
			   | [Generative: String]
			   | [Effects: String]
			   
example-expr = racket-expr                  ; Example of a piece of data
		     | racket-expr => racket-expr   ; Example of evaluation
		     | racket-expr <= String        ; Description of a piece of data

type-expr = id
		  | (id type-expr ...)
		  | [Union type-expr ...+]
		  | [Intersection type-expr ...+]
		  | [Listof type-expr]
		  | [Maybe type-expr]
		  | type-expr ... -> type-expr
		  | provided-type-expr

provided-type-expr = Void
				   | Bool
				   | PosInt
				   | NegInt
				   | Decimal
				   | Char
				   | String
				   | Any
				   | Nat
				   | Int
				   | Num
				   | Unknown
				   | Nothing
```

### define-data

```Scheme
(define-data name
	[: - type-expr
	   ...+]
	[Intepretation: description]
	extras ...)
```

Creates a documentation entry at phase 1, as well as a new type, with `name` and the given expression. The new data is defined in the scope of any documentation forms in the rest of the program. This entry compiles to a data definition, with `description` going below the data-definition, alongside any extra documentation props (`extras`).

### define-docs

**Function Definitions:**

```Scheme
(define-docs (name args ...)
	[Signature: type(s) -> output-type]
	[Purpose: description]
	extras ...) ; No repeats of same type of doc
```

Creates a documentation entry at phase 1 with ``name`` and the given signature and purpose statement. Upon compiling, each of the `type(s)` will be paired with corresponding `arg` in  `args` in the scribble output. If there are missing types, they will be filled in with "???". The `description` will go below the function definition, alongside any extra documentation props (`extras`).

**Constant Definitions:**

```Scheme
(define-docs name
	[Signature: type]
	[Purpose: description]
	extras ...) ; No repeats of same type of doc
```

Creates a documentation entry at phase 1 with `name` and the given signature and purpose statement. When compiled, he `description` will go below the constant definition, alongside any extra documentation props (`extras`).

**Macro Definitions:**

```Scheme
(define-docs name
	[Syntax: stx]
	[Semantics: description]
	extras ...) ; No repeats of same type of doc
```

Creates a documentation entry at phase 1 with `name` and the given syntax-expression and semantics-statement. This documentation entry is compiled as a form of the shape `stx`. The `description` will go below the macro definition, alongside any extra documentation props (`extras`).

## Compiling Racket-Docs

Compiling the completed *Racket-Docs* program can be done at the press of the compile docs button: ![](img/button.png) in the DrRacket GUI. Alternatively, the documentation can be compiled using the command `compile-docs` at the syntax-level (racket-docs forms are at phase 1).

```scheme
(define-docs compile-docs
  [Syntax: (compile-docs (get-all-docs) path-expr)]
  [Semantics: "Compiles all the documentation to the specified path (defaults to temp if no path-expr is given)"])
```

This uses the `get-all-docs` command, which simply retrieves all the documentation entries *so far* and returns a list of entries.

```scheme
(define-docs get-all-docs
  [Syntax: (get-all-docs)]
  [Semantics: "Retrieves all defined documentation entries so far"])
```

