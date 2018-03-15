#lang racket ; typed-racket causes weird errors for syntax docs

(require "doc-prop.rkt")

(provide [struct-out doc-entry]
         val-doc-entry
         macro-doc-entry
         type-doc-entry)

; A DocEntry is (doc-entry DocType Syntax [Listof DocProp])
; INTERPRETATION Documents a single value, macro, or datatype.
; A DocType is one of:
; - 'value
; - 'macro
; - 'type
; INTERPRETATION Describes what type of data a doc entry documents.
; - 'value - It documents a value.
; - 'macro - It documents a macro.
; - 'type - It documents a datatype.

(struct doc-entry [type id props] #:transparent)

; Syntax [Listof DocProp] -> DocEntry
; Creates a doc entry which documents a value.
(define (val-doc-entry id props)
  (doc-entry 'value id props))

; Syntax [Listof DocProp] -> DocEntry
; Creates a doc entry which documents a macro.
(define (macro-doc-entry id props)
  (doc-entry 'macro id props))

; Syntax [Listof DocProp] -> DocEntry
; Creates a doc entry which documents a datatype.
(define (type-doc-entry id props)
  (doc-entry 'type id props))