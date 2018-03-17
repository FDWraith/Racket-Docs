#lang typed/racket

(require "struct.rkt")
(require/typed "struct.rkt"
               [#:struct doc-entry ([type : (U 'value 'macro 'type)] [id : Any] [props : [Listof doc-prop]])]
               )


#|
;; Figure out where this compile-time function is being called from ???
(define out (open-output-file "hello.scrbl"))
(write "hello world" out)
(close-output-port out)
|#

; Effect: Generates a Scribble File from the documentation
(: compile-docs (-> Any Void))
(define (compile-docs all-docs)
  (local
    ((define out (open-output-file "hello.scrbl"))
     (define (print/s arg) (print arg out)))
    #;(map (compose print/s compile-doc-entry) all-docs)
    (void)))


; DocEntry -> String
; Compiles the document entry to valid Scribble line(s)
(: compile-doc-entry (-> doc-entry String))
(define (compile-doc-entry ent)
  (local
    ((define ent-type (doc-entry-type ent)))
    (cond
      [(symbol=? ent-type 'type) (compile-doc-data ent)]
      [(symbol=? ent-type 'value) (compile-doc-const ent)]
      [(symbol=? ent-type 'macro) (compile-doc-func ent)])))

; DocEntry -> String
; Compiles Data Definitions to valid Scribble line(s)
(: compile-doc-data (-> doc-entry String))
(define (compile-doc-data dat)
  (local
    ((define dat-type (doc-entry-id dat))
     (define dat-body (doc-entry-props dat))
     (define type-prop? (mk-prop? 'type))
     (define type-prop (extract type-prop? dat-body))
     (define type (if (empty? type-prop)
                      (error "type not found")
                      (doc-prop-value type-prop)))
     (define desc-prop? (mk-prop? 'desc))
     (define desc-prop (extract desc-prop? dat-body))
     (define desc (if (empty? desc-prop)
                      ""
                      (doc-prop-value desc-prop))))
    ""))

; DocEntry -> String
; Compiles Constants to valid Scribble line(s)
(: compile-doc-const (-> doc-entry String))
(define (compile-doc-const ent)
  "")

; DocEntry -> String
; Compiles Functions to valid Scribble line(s)
(: compile-doc-func (-> doc-entry String))
(define (compile-doc-func ent)
  "")


; DocPropType -> [DocProp -> Boolean]
; Creates a function that determines if a given DocProp
; matches that type
(: mk-prop? (-> Doc-Prop-Type [-> doc-prop Boolean]))
(define (mk-prop? type )
  (λ (prop) (prop-type=? type (doc-prop-type prop))))

  
; [doc-prop -> Boolean] [Listof doc-prop] -> [U doc-prop '()]
; Returns the first element in the list that matches pred
; Returns an empty list if no such element is found
(: extract (-> (-> doc-prop Boolean) [Listof doc-prop] (U doc-prop '())))
(define (extract pred lst)
  '())
    


