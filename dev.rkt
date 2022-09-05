#lang racket

























; open input file
(define in (open-input-file "src1.txt" #:mode 'text))

; summary: Takes an input text file and turn it into a list of characters
; param: input - already open input file stream
; returns: The newly created list of every character as strings, else '$$' for eof
(define (print-file input)
  (define char (read-string 1 input))
  (cond [(eof-object? char) "$$"]
        [else (print char)
              (print-file input)]))

(print-file in)

; close input stream
(close-input-port in)

; open input file
(define in2 (open-input-file "src1.txt" #:mode 'text))

; summary: Takes an input stream and converts the file into a string
; param: input - the input stream
; returns: Either a "$$" for an empty file or the file's contents as a single string
(define (build-string input)
  (define char (read-string 1 input))
  (cond [(eof-object? char) "$$"]
        [(string-append char (build-string input))]))

(print (build-string in2))

; close input stream
(close-input-port in2)
