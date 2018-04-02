#lang racket

(provide parse-raw-text)

#;(define-docs (parse-raw-text str)
    [Signature: String -> RawText]
    [Purpose: "Converts @str from Racket-docs raw text into Scribble text."])
(define (parse-raw-text str)
  (regexp-replace* #rx"@([^ ']*)"
                   (regexp-replace* #rx"@([^ '@]*)@"
                                    str
                                    "@racket[\\1]")
                   "@racket[\\1]"))