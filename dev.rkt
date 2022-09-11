#lang racket
(require racket/match)

; open input file
(define in (open-input-file "src1.txt" #:mode 'text))

;want a fcn that takes a string and returns the pattern that it matches
(define (match-pattern input)
  exit)

(define (build-token input)
  (cond [(eof-object? (mcar input) "$$")]
        [(string=? (mcar input) " ") ""]
        [(string=? (mcar input) "\n") (+ (mcdr input) 1) ""]))

; summary: Takes an input stream and read from it until it encounters a whitespace or newline or eof
;          marker. Then it returns that 'token'.
(define (make-token input)
  (define lineNumber 1)
  (define char (read-string 1 input))
  (build-token (mcons char lineNumber))
  ()

; summary: Designed to work with build-token. Prints every 'token' from
;          an input stream. It grabs a token one at a time and prints.
(define (print-tokens in)
  (define token (build-token in))
  (cond [(string=? token "$$") (print token)]
        [(print token) (print-tokens in)]))

;(print-tokens in)

(print (make-token in))
  
; close input stream
(close-input-port in)
