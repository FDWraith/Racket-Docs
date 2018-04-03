#lang racket-docs

(define-docs VOWELS
  [Signature: [Listof Char]]
  [Purpose: "All the vowels in the alphabet, in lowercase."])
(define VOWELS (list #\a #\e #\i #\o #\u #\y))

(define-docs (shorthand str)
  [Signature: String -> String]
  [Purpose: "Removes vowels from @str."]
  [Examples: (shorthand "") => ""
             (shorthand "world") => "wrld"
             (shorthand "ApPle;") => "pPl;"])
(define (shorthand str)
  (local
    [(define-docs (shorthand-chars chars)
       [Signature: [Listof Char] -> [Listof Char]]
       [Purpose: "Removes vowels from the character list."])
     (define (shorthand-chars chars)
       (cond [(empty? chars) '()]
             [(cons? chars)
              (if (member (char-downcase (first chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))