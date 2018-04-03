# Racket-Docs

## Purpose

*Racket-Docs* is a documentation language for Racket. Its primary purpose is to allow users to document code at the same time that they are writing it. After the programmer is done typing, the documentation can be compiled into `HTML` at the press of a button. In addition, *Racket-Docs* also features type-checking and examples-checking in its documentation forms, so that the programmer can ensure that his/her code match up to what he/she designed.

## Vocabulary and Grammar

*Racket-Docs* introduces two new forms, `define-data` and `define-docs` that are used to define data-definitions and function-documentation.

```text
#lang racket-docs

racket-doc-expr = racket-expr ; Expressions From Racket
				| doc-expr 
				
doc-expr = (define-docs head-expr
			[Signature: type-expr]
			[Purpose: String]
			data-extra-expr ...) ; No repeats of same type of doc
	     | (define-data id
	        [: type-expr] ...+
	        [Interpretation: String]
	        extra-doc-expr ...) ; No repeats of same type of doc
	     | (define-docs id
	        [Syntax: racket-expr]
	        [Semantics: String]
	        extra-doc-expr ...) ; No repeats of same type of doc
	        
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

## Compiling Racket-Docs

Compiling the completed *Racket-Docs* program can be done at the press of the compile docs button: ![](img/button.png) in the DrRacket GUI. Alternatively, the documentation can be compiled using the command `compile-docs` at the syntax-level.

```text
(define-docs compile-docs
  [Syntax: (compile-docs (get-all-docs) path-expr)]
  [Semantics: "Compiles all the documentation to the specified path (defaults to temp if no path-expr is given"])
```

