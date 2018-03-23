#lang racket

(require (for-syntax "struct.rkt"
                     "utils.rkt"
                     racket
                     syntax/parse))
(require "struct.rkt")
(require "utils.rkt")
#;(require/typed "struct.rkt"
                 [#:struct doc-entry [(type : (U 'type 'macro 'value)) (id : Any) (props : [Listof doc-prop])]]
                 )
#;(require/typed "utils.rkt"
                 [syntax->string [Any -> String]]
                 )

#|
;; Figure out where this compile-time function is being called from ???
(define out (open-output-file "hello.scrbl"))
(write "hello world" out)
(close-output-port out)
|#

; Effect: Generates a Scribble File from the documentation
; (: compile-docs (-> (Listof doc-entry) Void))
(define-syntax (compile-docs stx)
  (syntax-parse stx
    [(_ all-docs)
     #'(begin
         (define out (open-output-file "hello.scrbl" #:exists 'truncate/replace))
         (display "#lang scribble/manual" out)
         (display (string-join (map compile-doc-entry all-docs) "\n") out)
         (close-output-port out))]))


; DocEntry -> String
; Compiles the document entry to valid Scribble line(s)
; (: compile-doc-entry (-> doc-entry String))
#;(define-syntax (compile-doc-entry stx)
    (syntax-parse stx
      [(_ ent)
       #'(begin
           (define ent-type (doc-entry-type ent))
           (cond
             [(symbol=? ent-type 'type) (compile-doc-data ent)]
             [(symbol=? ent-type 'value) (compile-doc-const ent)]
             [(symbol=? ent-type 'macro) (compile-doc-func ent)]))]
      [else (displayln stx)]))
(define (compile-doc-entry ent)
  (local
    ((define ent-type (doc-entry-type ent)))
    (cond
      [(symbol=? ent-type 'type) (compile-doc-data ent)]
      [(symbol=? ent-type 'value) (compile-doc-const ent)]
      [(symbol=? ent-type 'macro) (compile-doc-func ent)])))
     
; DocEntry -> String
; Compiles Data Definitions to valid Scribble line(s)
; (: compile-doc-data (-> doc-entry String))
(define (compile-doc-data dat)
  (local
    ((define dat-type (doc-entry-id dat))
     (define dat-body (doc-entry-props dat))
     (define type-prop? (mk-prop? 'type))
     (define type-prop (extract type-prop? dat-body))
     (define type
       (cond
         [(doc-prop? type-prop) (syntax->string (doc-prop-value type-prop))]
         [else (error "type not found")]))
     (define desc-prop? (mk-prop? 'desc))
     (define desc-prop (extract desc-prop? dat-body))
     (define desc (if (empty? desc-prop)
                      ""
                      (syntax->string (doc-prop-value desc-prop))))
     (define example-prop? (mk-prop? 'examples))
     (define example-prop (extract example-prop? dat-body)))
    (string-append "Type:" type "\n" "Interpretation:" desc)))

; DocEntry -> String
; Compiles Constants to valid Scribble line(s)
; (: compile-doc-const (-> doc-entry String))
(define-syntax (compile-doc-const ent)
  #'"")

; DocEntry -> String
; Compiles Functions to valid Scribble line(s)
; (: compile-doc-func (-> doc-entry String))
(define-syntax (compile-doc-func ent)
  #'"")

; DocEntry -> String
; Compiles Macros to valid Scribble line(s)
; (: compile-doc-macro (-> doc-entry String))
(define-syntax (compile-doc-macro ent)
  #'"")


; DocPropType -> [DocProp -> Boolean]
; Creates a function that determines if a given DocProp
; matches that type
; (: mk-prop? (-> Doc-Prop-Type [-> doc-prop Boolean]))
(define (mk-prop? type)
  (λ (prop) (prop-type=? type (doc-prop-type prop))))


; [doc-prop -> Boolean] [Listof doc-prop] -> [U doc-prop '()]
; Returns the first element in the list that matches pred
; Returns an empty list if no such element is found
; (: extract (-> (-> doc-prop Boolean) [Listof doc-prop] (U doc-prop '())))
(define (extract pred lst)
  (cond
    [(empty? lst) lst]
    [else (let ([fst (first lst)])
            (if (pred fst) fst (extract pred (rest lst))))]))
