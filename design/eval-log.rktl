;; @ Monday, April 2nd, 2018 11:35:22pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/racket-docs-code.rkt
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
  (local [(define-docs (shorthand-chars chars)
            [Signature: [Listof Char] -> [Listof Char]]
            [Purpose: "Removes vowels from the character list."])
          (define (shorthand-chars chars)
            (cond [(empty? chars) '()]
                  [(cons? chars) (if (member (char-downcase (first chars)) VOWELS)
                                     (shorthand-chars (rest chars))
                                     (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))

;; @ Monday, April 2nd, 2018 11:35:35pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/racket-docs-code.rkt
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
  (local [(define-docs (shorthand-chars chars)
            [Signature: [Listof Char] -> [Listof Char]]
            [Purpose: "Removes vowels from the character list."])
          (define (shorthand-chars chars)
            (cond [(empty? chars) '()]
                  [(cons? chars) (if (member (char-downcase (first chars)) VOWELS)
                                     (shorthand-chars (rest chars))
                                     (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
(println 5)

;; @ Monday, April 2nd, 2018 11:35:37pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/racket-docs-code.rkt
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
  (local [(define-docs (shorthand-chars chars)
            [Signature: [Listof Char] -> [Listof Char]]
            [Purpose: "Removes vowels from the character list."])
          (define (shorthand-chars chars)
            (cond [(empty? chars) '()]
                  [(cons? chars) (if (member (char-downcase (first chars)) VOWELS)
                                     (shorthand-chars (rest chars))
                                     (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
(println 5)

;; @ Monday, April 2nd, 2018 11:35:38pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/racket-docs-code.rkt
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
  (local [(define-docs (shorthand-chars chars)
            [Signature: [Listof Char] -> [Listof Char]]
            [Purpose: "Removes vowels from the character list."])
          (define (shorthand-chars chars)
            (cond [(empty? chars) '()]
                  [(cons? chars) (if (member (char-downcase (first chars)) VOWELS)
                                     (shorthand-chars (rest chars))
                                     (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
(println 5)

;; @ Monday, April 2nd, 2018 11:35:51pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/racket-docs-code.rkt
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
  (local [(define-docs (shorthand-chars chars)
            [Signature: [Listof Char] -> [Listof Char]]
            [Purpose: "Removes vowels from the character list."])
          (define (shorthand-chars chars)
            (cond [(empty? chars) '()]
                  [(cons? chars) (if (member (char-downcase (rest chars)) VOWELS)
                                     (shorthand-chars (rest chars))
                                     (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))

;; @ Monday, April 2nd, 2018 11:35:57pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/racket-docs-code.rkt
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
  (local [(define-docs (shorthand-chars chars)
            [Signature: [Listof Char] -> [Listof Char]]
            [Purpose: "Removes vowels from the character list."])
          (define (shorthand-chars chars)
            (cond [(empty? chars) '()]
                  [(cons? chars) (if (member (char-downcase (first chars)) VOWELS)
                                     (shorthand-chars (rest chars))
                                     (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))

;; @ Monday, April 2nd, 2018 11:42:07pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/isl-code.rkt
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
              (if (member (char-downcase (rest chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
;; @ Monday, April 2nd, 2018 11:42:16pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/isl-code.rkt
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
              (if (member (char-downcase (rest chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
;; @ Monday, April 2nd, 2018 11:42:20pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/isl-code.rkt
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
              (if (member (char-downcase (rest chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
;; @ Monday, April 2nd, 2018 11:42:22pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/isl-code.rkt
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
              (if (member (char-downcase (rest chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
;; @ Monday, April 2nd, 2018 11:42:39pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/isl-code.rkt
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
              (if (member (char-downcase (rest chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
(println (shorthand "ApPle;"))
;; @ Monday, April 2nd, 2018 11:42:49pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/isl-code.rkt
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
              (if (member (char-downcase (rest chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
(shorthand "ApPle;")
;; @ Monday, April 2nd, 2018 11:46:12pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/isl-code.rkt
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
;; @ Monday, April 2nd, 2018 11:46:34pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/isl-code.rkt
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
              (if (member (char-downcase (rest chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
(shorthand "Hello")
;; @ Monday, April 2nd, 2018 11:47:21pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/isl-code.rkt
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
              (if (member (char-downcase (rest chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
;; @ Monday, April 2nd, 2018 11:47:30pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/isl-code.rkt
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
              (if (member (char-downcase (rest chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
(shorthand "Apple")
;; @ Monday, April 2nd, 2018 11:47:32pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/isl-code.rkt
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
              (if (member (char-downcase (rest chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
(shorthand "Apple")
;; @ Monday, April 2nd, 2018 11:48:00pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/racket-docs-code.rkt
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
              (if (member (char-downcase (rest chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
;; @ Monday, April 2nd, 2018 11:48:15pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/racket-docs-code.rkt
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
    (shorthand-chars (string->list str))))
;; @ Monday, April 2nd, 2018 11:48:33pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/racket-docs-code.rkt
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
    (shorthand-chars (string->list str))))
;; @ Monday, April 2nd, 2018 11:48:38pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/racket-docs-code.rkt
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
    (shorthand-chars str)))
;; @ Monday, April 2nd, 2018 11:48:52pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/racket-docs-code.rkt
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
  (define-docs (shorthand-chars chars)
    [Signature: [Listof Char] -> [Listof Char]]
    [Purpose: "Removes vowels from the character list."])
  (define (shorthand-chars chars)
    (cond [(empty? chars) '()]
          [(cons? chars)
           (if (member (char-downcase (first chars)) VOWELS)
               (shorthand-chars (rest chars))
               (cons (first chars) (shorthand-chars (rest chars))))]))
    (shorthand-chars str))
;; @ Monday, April 2nd, 2018 11:49:03pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/racket-docs-code.rkt
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
  (shorthand-chars str))

(define-docs (shorthand-chars chars)
  [Signature: [Listof Char] -> [Listof Char]]
  [Purpose: "Removes vowels from the character list."])
(define (shorthand-chars chars)
  (cond [(empty? chars) '()]
        [(cons? chars)
         (if (member (char-downcase (first chars)) VOWELS)
             (shorthand-chars (rest chars))
             (cons (first chars) (shorthand-chars (rest chars))))]))
;; @ Monday, April 2nd, 2018 11:57:26pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/isl-code.rkt
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
    [; [Listof Char] - [Listof Char]
     ; Removes vowels from the character list.
     (define (shorthand-chars chars)
       (cond [(empty? chars) '()]
             [(cons? chars)
              (if (member (char-downcase (first chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
;; @ Monday, April 2nd, 2018 11:58:07pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/racket-docs-code.rkt
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
              (if (member (char-downcase (rest chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
;; @ Monday, April 2nd, 2018 11:58:12pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/racket-docs-code.rkt
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
              (if (member (char-downcase (rest chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
;; @ Monday, April 2nd, 2018 11:58:51pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/isl-code.rkt
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
(shorthand "ApPle")
;; @ Monday, April 2nd, 2018 11:59:03pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/isl-code.rkt
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
              (if (member (char-downcase (rest chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
(shorthand "ApPle")
;; @ Monday, April 2nd, 2018 11:59:10pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/isl-code.rkt
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
    [; [Listof Char] - [Listof Char]
     ; Removes vowels from the character list.
     (define (shorthand-chars chars)
       (cond [(empty? chars) '()]
             [(cons? chars)
              (if (member (char-downcase (rest chars)) VOWELS)
                  (shorthand-chars (rest chars))
                  (cons (first chars) (shorthand-chars (rest chars))))]))]
    (list->string (shorthand-chars (string->list str)))))
(shorthand "ApPle")
;; @ Monday, April 2nd, 2018 11:59:17pm -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/racket-docs-code.rkt
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
;; @ Tuesday, April 3rd, 2018 12:01:27am -----------
;; /Users/jakob/Dropbox/College/CS4620 HYOL/DocLang/Racket-Docs/design/racket-docs-code.rkt
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
