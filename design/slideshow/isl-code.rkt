#lang htdp/isl

; [Listof Char] - All the vowels in the alphabet, in lowercase.
(define VOWELS (list #\a #\e #\i #\o #\u #\y))

; String -> String
; Removes vowels from str.
(check-expect (shorthand "") "")
(check-expect (shorthand "world") "wrld")
(check-expect (shorthand "ApPle;") "pPl;")
(define (shorthand str)
  (local
    [; [Listof Char] -> [Listof Char]
     ; Removes vowels from the character list.
     (define (shorthand-chars chars)
       (cond [(empty? chars) '()]
             [(cons? chars)
              (if (member (char-downcase (first chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))

(shorthand "Racket")