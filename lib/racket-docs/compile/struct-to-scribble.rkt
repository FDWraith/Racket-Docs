#lang racket

(require  "../struct.rkt"
          "../types.rkt"
          "../utils.rkt"
          "../parse.rkt")

(provide compile-docs)

; Effect: Generates a Scribble File from the documentation
(define (compile-docs docs [name "temp"])
  (define path (string-append name ".scrbl"))
  (define (mk-ent-type? type)
    (λ (ent) (symbol=? (doc-entry-type ent) type)))
  (define (mk-ent-string type)
    (string-join (map compile-doc-entry (filter (mk-ent-type? type) docs))
                 "\n"))
  (define section-tags
    (list "datadefs"
          "constants"
          "functions"
          "macros"))
  (define section-labels
    (list "Data Definitions"
          "Constants"
          "Functions"
          "Macros"))
  (define section-types
    (list 'type 'const 'func 'macro))
  (define out-sections
    (map (λ (tag label type)
           (string-append "@section[#:tag \""
                          tag
                          "\"]{"
                          label
                          "}\n"
                          (mk-ent-string type)))
         section-tags
         section-labels
         section-types))
  (define out-string (string-join out-sections "\n"))
  (define out (open-output-file path #:exists 'truncate))
  (display "#lang scribble/manual\n\n" out)
  (display "@require[scribble/example]\n\n" out)
  (fprintf out "@title{~a Documentation}\n" (string-titlecase name))
  (for ([tag section-tags])
    (fprintf out "@secref{~a}\n\n" tag))
  (display out-string out)
  (close-output-port out))

; Compiles Doc Entries to valid Scribble line(s)
(define (compile-doc-entry ent)
  (define ent-type (doc-entry-type ent))
  (cond
    [(symbol=? ent-type 'type) (compile-doc-data ent)]
    [(symbol=? ent-type 'func) (compile-doc-func ent)]
    [(symbol=? ent-type 'const) (compile-doc-const ent)]
    [(symbol=? ent-type 'macro) (compile-doc-macro ent)]))

; Compiles Data Definitions to valid Scribble line(s)
(define (compile-doc-data dat)
  (define dat-type (syntax->string (doc-entry-id dat)))
  (define props (doc-entry-props dat))
  (define type-prop (extract (mk-prop? 'type) props))
  (define type (doc-prop-value type-prop))
  (define type-string (wrap-safe (string-join (type-label/union type) "\n")))
  (define desc-prop (extract (mk-prop? 'desc) props))
  (define desc (doc-prop-value desc-prop))  
  (define example-prop (extract (mk-prop? 'examples) props))
  (define examples (if (empty? example-prop) ""
                       (compile-doc-examples (doc-prop-value example-prop))))
  (string-append "@defthing[#:kind \"data defintion\" #:link-target? #f "
                 dat-type
                 " Type #:value "
                 type-string
                 "]{\n"
                 desc "\n"
                 "}\n\n"
                 examples "\n"))

; Compiles Identifiers to valid Scribble line(s)
(define (compile-doc-const ent)
  (define name (syntax->string (doc-entry-id ent)))
  (define props (doc-entry-props ent))
  (define desc-prop (extract (mk-prop? 'desc) props))
  (define type-prop (extract (mk-prop? 'type) props))
  (define type (safe-type-label (doc-prop-value type-prop)))
  (define desc (doc-prop-value desc-prop))
  (define example-prop (extract (mk-prop? 'examples) props))
  (define accumulator-prop (extract (mk-prop? 'accumulator) props))
  (define generative-prop (extract (mk-prop? 'generative) props))
  (define effects-prop (extract (mk-prop? 'effects) props))
  (define examples (if (empty? example-prop) ""
                       (compile-doc-examples (doc-prop-value example-prop))))
  (define accumulator (if (empty? accumulator-prop) ""
                          (compile-doc-accumulator (doc-prop-value accumulator-prop))))
  (define generative (if (empty? generative-prop) ""
                         (compile-doc-generative (doc-prop-value generative-prop))))
  (define effects (if (empty? effects-prop) ""
                      (compile-doc-effects (doc-prop-value effects-prop))))
  (string-append "@defthing[#:kind \"constant\" #:link-target? #f "
                 name
                 " "
                 type
                 "]{\n"
                 desc
                 "\n}\n\n"
                 examples "\n"
                 accumulator "\n"
                 generative "\n"
                 effects "\n"))

; Compiles Functions to valid Scribble line(s)
(define (compile-doc-func ent)
  (define name (syntax->string (doc-entry-id ent)))
  (define props (doc-entry-props ent))
  (define args-prop (extract (mk-prop? 'args) props))
  (define args (rest (map syntax->string (syntax->list (doc-prop-value args-prop)))))
  (define type-prop (extract (mk-prop? 'type) props))
  (define func-type (fill-forall (doc-prop-value type-prop)))
  (define param-types (try-func-params func-type))
  (define param-type-labels (map safe-type-label param-types))
  (define args-info
    (and param-types
         (map (λ (arg type-label) (string-append "[" arg " " type-label "]"))
              (pad-right args "???" (length param-type-labels))
              (pad-right param-type-labels "???" (length args)))))
  (define args-string
    (if args-info
        (string-join args-info " ")
        ""))
  (define out-type (try-func-out func-type))
  (define output
    (if out-type
        (safe-type-label out-type)
        "???"))
  (define desc-prop (extract (mk-prop? 'desc) props))
  (define purp (doc-prop-value desc-prop))
  (define example-prop (extract (mk-prop? 'examples) props))
  (define accumulator-prop (extract (mk-prop? 'accumulator) props))
  (define generative-prop (extract (mk-prop? 'generative) props))
  (define effects-prop (extract (mk-prop? 'effects) props))
  (define examples (if (empty? example-prop) ""
                       (compile-doc-examples (doc-prop-value example-prop))))
  (define accumulator (if (empty? accumulator-prop) ""
                          (compile-doc-accumulator (doc-prop-value accumulator-prop))))
  (define generative (if (empty? generative-prop) ""
                         (compile-doc-generative (doc-prop-value generative-prop))))
  (define effects (if (empty? effects-prop) ""
                      (compile-doc-effects (doc-prop-value effects-prop))))
  (string-append "@defproc[#:link-target? #f ("
                 name " "
                 args-string ") "
                 output "]{\n"
                 purp "\n"
                 "\n}\n\n"
                 examples "\n"
                 accumulator "\n"
                 generative "\n"
                 effects "\n"))

; Compiles Macros to valid Scribble line(s)
(define (compile-doc-macro ent)
  (define name (syntax->string (doc-entry-id ent)))
  (define props (doc-entry-props ent))
  (define stx-prop (extract (mk-prop? 'syntax) props))
  (define sem-prop (extract (mk-prop? 'desc) props))
  (define stx (syntax->string (doc-prop-value stx-prop)))
  (define sem (doc-prop-value sem-prop))
  (define example-prop (extract (mk-prop? 'examples) props))
  (define accumulator-prop (extract (mk-prop? 'accumulator) props))
  (define generative-prop (extract (mk-prop? 'generative) props))
  (define effects-prop (extract (mk-prop? 'effects) props))
  (define examples (if (empty? example-prop) ""
                       (compile-doc-examples (doc-prop-value example-prop))))
  (define accumulator (if (empty? accumulator-prop) ""
                          (compile-doc-accumulator (doc-prop-value accumulator-prop))))
  (define generative (if (empty? generative-prop) ""
                         (compile-doc-generative (doc-prop-value generative-prop))))
  (define effects (if (empty? effects-prop) ""
                      (compile-doc-effects (doc-prop-value effects-prop))))
  (string-append "@defform[#:link-target? #f #:id "
                 name " " stx " ]{\n"
                 sem 
                 "\n}\n\n"
                 examples "\n"
                 accumulator "\n"
                 generative "\n"
                 effects "\n"))

; Compiles Examples
(define (compile-doc-examples loe)
  (define (compile-example ex)
    (cond
      [(plain-data-example? ex)
       (format "@racketblock[~a]\n"
               (wrap-safe-empty (syntax->string (plain-data-example-expr ex))))]
      [(interpret-data-example? ex)
       (format "@racketblock[~a (code:comment ~v)]\n"
               (wrap-safe-empty (syntax->string (interpret-data-example-expr ex)))
               (interpret-data-example-interpretation ex))]
      [(eval-example? ex)
       (format "@examples[#:label #false (eval:alts ~a (eval:result (racket ~a) \"\" \"\"))]\n"
               (wrap-safe-empty (syntax->string (eval-example-expr ex)))
               (wrap-safe-empty (syntax->string (eval-example-expected ex))))]))
  (string-append
   "@bold{Examples:}\n"
   (string-join (map compile-example loe) "\n"
                #:after-last "\n")))

; Compiles Accumulator Statement
(define (compile-doc-accumulator acc)
  (format "@bold{Accumulator} - @racket[~a]:\n~a"
          (syntax->string (accumulator-id acc))
          (accumulator-desc acc)))

; Compiles Generative Statement
(define (compile-doc-generative gen-desc)
  (format "@bold{Generative:}\n~a" gen-desc))

; Compiles Effects Statement
(define (compile-doc-effects effects-desc)
  (format "@bold{Effects:}\n~a" effects-desc))

; Type -> String
; The label of a type, wrapped in (code:line ...),
; so it will be considered a single argument even if the type has spaces.
(define (safe-type-label x)
  (wrap-safe (type-label x)))

; String -> String
; Makes the string considered a single argument even if it's empty or has spaces.
(define (wrap-safe x)
  (format "(code:line ~a)" x))

; String -> String
; Makes the string considered a single argument even if it's empty.
(define (wrap-safe-empty x)
  (cond
    [(string=? x "") "code:space"]
    [else x]))