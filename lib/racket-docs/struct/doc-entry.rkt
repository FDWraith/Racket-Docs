#lang racket

(require "doc-prop.rkt")

(provide [struct-out doc-entry]
         val-doc-entry
         macro-doc-entry
         type-doc-entry)

#;(define-data DocEntry
    [: (doc-entry DocType Syntax [Listof DocProp])]
    [Interpretation: "Documents a single value, macro, or datatype."])
(struct doc-entry [type id props] #:transparent)

#;(define-data DocType
    [: - 'value
       - 'macro
       - 'type]
    [Interpretation: #<<"
Describes what type of data a doc entry documents.
- 'value - It documents a value.
- 'macro - It documents a macro.
- 'type - It documents a datatype.
"
                     ])

#;(define-docs val-doc-entry
    [Signature: Syntax [Listof DocProp] -> DocEntry]
    [Purpose: "Creates a doc entry which documents a value."]
    [Examples:
     (val-doc-entry #'+ (list (doc-prop 'desc "Adds 2 numbers.")))])
(define (val-doc-entry id props)
  (doc-entry 'value id props))

#;(define-docs val-doc-entry
    [Signature: Syntax [Listof DocProp] -> DocEntry]
    [Purpose: "Creates a doc entry which documents a macro."]
    [Examples:
     (macro-doc-entry
      #'quote (list (doc-prop 'desc "Turns the contents into symbols.")))])
(define (macro-doc-entry id props)
  (doc-entry 'macro id props))

#;(define-docs val-doc-entry
    [Signature: Syntax [Listof DocProp] -> DocEntry]
    [Purpose: "Creates a doc entry which documents a datatype."]
    [Examples:
     (type-doc-entry #'String
                     (list (doc-prop 'desc "A sequence of characters.")))])
(define (type-doc-entry id props)
  (doc-entry 'type id props))