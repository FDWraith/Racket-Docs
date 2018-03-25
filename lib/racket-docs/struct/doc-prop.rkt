#lang racket

(provide [struct-out doc-prop]
         args-doc-prop
         type-doc-prop
         sig-doc-prop
         syntax-doc-prop
         desc-doc-prop
         interpretation-doc-prop
         purpose-doc-prop
         semantics-doc-prop
         examples-doc-prop
         accumulator-doc-prop
         generative-doc-prop
         effects-doc-prop
         prop-has-type?
         prop-type=?
         prop-type->string
         check-shared-types)

#;(define-data DocProp
    [: (doc-prop DocPropType Any)]
    [Interpretation:
     "Documents something about a term - it's type, purpose, examples, ..."]
    [Examples:
     "It's a string." <= (doc-prop 'type #'String)
     "It writes to a file." <= (doc-prop 'desc "Writes to a file.")
     "It's foo+, and called with 5 and 7, it returns 12" <=
     (doc-prop 'examples (list (eval-example #'(foo+ 5 7) 12)))])
(struct doc-prop [type value] #:transparent)

#;(define-data DocPropType
    [: - 'args
       - 'type
       - 'syntax
       - 'desc
       - 'examples
       - 'accumulator
       - 'generative
       - 'effects]
    [Interpretation:
     "What a doc prop means - whether its a description or accumulator or ..."])

#;(define-docs args-doc-prop
    [Signature: [Maybe [Stx List]] -> DocProp]
    [Purpose: #<<"
Documents the arguments given to a function.
To document an identifier (no arguments), pass #false.
"
              ])
(define (args-doc-prop args)
  (doc-prop 'args args))

#;(define-docs type-doc-prop
    [Signature: [Stxof Type] -> DocProp]
    [Purpose: "Documents that the term has the given type."])
(define (type-doc-prop type)
  (doc-prop 'type type))

#;(define-docs sig-doc-prop
    [Signature: [Stxof Type] -> DocProp]
    [Purpose: "Documents that the term has the given signature."])
(define sig-doc-prop type-doc-prop)

#;(define-docs syntax-doc-prop
    [Signature: [Stxof [Listof Syntax]] -> DocProp]
    [Purpose: "Documents that the term has the given syntactic forms."])
(define (syntax-doc-prop syntax)
  (doc-prop 'syntax syntax))

#;(define-docs desc-doc-prop
    [Signature: Desc -> DocProp]
    [Purpose: "Documents that the term has the given description."])
(define (desc-doc-prop desc)
  (doc-prop 'desc desc))

#;(define-docs interpretation-doc-prop
    [Signature: Desc -> DocProp]
    [Purpose: "Documents that the (type) term has the given interpretation."])
(define interpretation-doc-prop desc-doc-prop)

#;(define-docs purpose-doc-prop
    [Signature: Desc -> DocProp]
    [Purpose: "Documents that the (value) term has the given purpose."])
(define purpose-doc-prop desc-doc-prop)

#;(define-docs semantics-doc-prop
    [Signature: Desc -> DocProp]
    [Purpose: "Documents that the (syntax) term has the given semantics."])
(define semantics-doc-prop desc-doc-prop)

#;(define-docs examples-doc-prop
    [Signature: [Listof Example] -> DocProp]
    [Purpose: "Documents the given examples, assuming they use the term."])
(define (examples-doc-prop examples)
  (doc-prop 'examples examples))

#;(define-docs accumulator-doc-prop
    [Signature: Accumulator -> DocProp]
    [Purpose: "Documents the given accumulator in the term."])
(define (accumulator-doc-prop accumulator)
  (doc-prop 'accumulator accumulator))

#;(define-docs generative-doc-prop
    [Signature: Desc -> DocProp]
    [Purpose: "Documents that the term uses the described generative scheme."])
(define (generative-doc-prop generative)
  (doc-prop 'generative generative))

#;(define-docs effects-doc-prop
    [Signature: Desc -> DocProp]
    [Purpose: "Documents that the term causes the described effects."])
(define (effects-doc-prop effects)
  (doc-prop 'effects effects))

#;(define-docs prop-has-type?
    [Signature: DocPropType DocProp -> Bool]
    [Purpose: "Is the doc prop of the given type?"])
(define (prop-has-type? type prop)
  (prop-type=? type (doc-prop-type prop)))

#;(define-docs prop-type=?
    [Signature: DocPropType DocPropType -> Bool]
    [Purpose: "Are the prop types equal?"])
(define prop-type=? symbol=?)

#;(define-docs prop-type->string
    [Signature: DocPropType -> String]
    [Purpose: "A label of the prop type, suitable for an error message."])
(define prop-type->string symbol->string)

#;(define-docs check-shared-types
    [Signature: [Listof DocProp] -> [Maybe DocPropType]]
    [Purpose: #<<"
Returns the first type found which is shared by 2 doc props.
If the doc props don't share any types, returns #false
"
              ])
(define (check-shared-types props)
  (check-duplicates (map doc-prop-type props)))