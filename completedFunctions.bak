#lang racket

; function archive

; summary: Takes an input stream and works with build-tokens to parse each chunk into
;          its proper identifier. For example anything that is a number is simply
;          converted into a "number", and anything that is an identifier into "id".
; notes: The grammar that this is for is the calculator grammar used by the parser assignment.
(define (print-tokens in)
  (define token (build-token in))
  (cond [(string=? token "$$") (print token)]
        [(string=? token "read") (print token) (print-tokens in)]
        [(string=? token "write") (print token) (print-tokens in)]
        [(string=? token ":=") (print token) (print-tokens in)]
        [(string=? token "+") (print token) (print-tokens in)]
        [(string=? token "-") (print token) (print-tokens in)]
        [(string=? token "*") (print token) (print-tokens in)]
        [(string=? token "/") (print token) (print-tokens in)]
        [(string=? token "(") (print token) (print-tokens in)]
        [(string=? token ")") (print token) (print-tokens in)]
        [(regexp-match #rx"^[0-9]" token) (print "number") (print-tokens in)]
        [else (print "id") (print-tokens in)]))

; summary: Takes an input text file and turn it into a list of characters
; param: input - already open input file stream
; returns: The newly created list of every character as strings, else '$$' for eof
(define (print-file input)
  (define char (read-string 1 input))
  (cond [(eof-object? char) "$$"]
        [else (print char)
              (print-file input)]))

; summary: Takes an input stream and converts the file into a string
; param: input - the input stream
; returns: Either a "$$" for an empty file or the file's contents as a single string
(define (build-string input)
  (define char (read-string 1 input))
  (cond [(eof-object? char) "$$"]
        [(string-append char (build-string input))]))

; summary: Takes an input stream and read from it until it encounte rs a whitespace or newline or eof
;          marker. Then it returns that 'token'
(define (build-token input)
  (define char (read-string 1 input))
  (cond [(eof-object? char) "$$"]
        [(string=? char " ") ""]
        [(string=? char "\n") ""]
        [(string-append char (build-token input))]))

; summary: designed to work with build-token. Prints every 'token' from
;          an input stream
(define (print-tokens in)
  (define token (build-token in))
  (cond [(string=? token "$$") (print token)]
        [(print token) (print-tokens in)]))

; summary: racket selection sort
(define (selectionSort arr)
  (cond [(empty? arr) '()]
        [else (define x0 (apply min arr))
              (cons x0 (selectionSort (remove x0 arr)))]))

