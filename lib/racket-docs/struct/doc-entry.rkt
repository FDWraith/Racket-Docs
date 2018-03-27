#lang racket

(require "doc-prop.rkt")

(provide [struct-out doc-entry]
         func-doc-entry
         macro-doc-entry
         type-doc-entry
         const-doc-entry)

#;(define-data DocEntry
    [: (doc-entry DocType Syntax [Listof DocProp])]
    [Interpretation: "Documents a single value, macro, or datatype."])
(struct doc-entry [type id props] #:transparent)

#;(define-data DocType
    [: - 'func
       - 'macro
       - 'type
       - 'const]
    [Interpretation: #<<"
Describes what type of data a doc entry documents.
- 'value - It documents a function.
- 'macro - It documents a macro.
- 'type - It documents a datatype.
- 'const - It documents an identifer
"
                     ])

#;(define-docs func-doc-entry
    [Signature: Syntax [Listof DocProp] -> DocEntry]
    [Purpose: "Creates a doc entry which documents a function."]
    [Examples:
     (val-doc-entry #'+ (list (doc-prop 'desc "Adds 2 numbers.")))])
(define (func-doc-entry id props)
  (doc-entry 'func id props))

#;(define-docs macro-doc-entry
    [Signature: Syntax [Listof DocProp] -> DocEntry]
    [Purpose: "Creates a doc entry which documents a macro."]
    [Examples:
     (macro-doc-entry
      #'quote (list (doc-prop 'desc "Turns the contents into symbols.")))])
(define (macro-doc-entry id props)
  (doc-entry 'macro id props))

#;(define-docs type-doc-entry
    [Signature: Syntax [Listof DocProp] -> DocEntry]
    [Purpose: "Creates a doc entry which documents a datatype."]
    [Examples:
     (type-doc-entry #'String
                     (list (doc-prop 'desc "A sequence of characters.")))])
(define (type-doc-entry id props)
  (doc-entry 'type id props))

#;(define-docs const-doc-entry
    [Signature: Syntax [Listof DocProp] -> DocEntry]
    [Purpose: "Creates a doc entry which documents an identifer."]
    [Examples:
     (const-doc-entry #'String
                     (list (doc-prop 'desc "A letter")))])
(define (const-doc-entry id props)
  (doc-entry 'const id props))
