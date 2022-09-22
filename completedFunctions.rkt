#lang racket

; function archive

(define (program input-token)
  (cond
    [(string=? input-token "id") (stmt-list input-token)]
    [(string=? input-token "read") (stmt-list input-token)]
    [(string=? input-token "write") (stmt-list input-token)]
    [(string=? input-token "$$") (match input-token "$$") #t]
    [else #f]))

(define (stmt-list input-token)
  (cond
    [(string=? input-token "id") #t]
    [(string=? input-token "read") #t]
    [(string=? input-token "write") #t]
    [else #f]))

(define (first-char input-string)
  (cond
    [(string=? input-string "") ""]
    [ else
      (substring input-string 0 1)]))

(define (rest-of-chars input-string)
  (if (>= (string-length input-string) 2) (substring input-string 1 (string-length input-string)) ""))

; summary: Takes a list of characters, outputs each token
(define (build-tokens input-line token)
  (cond
    ; ""
    [(and (string=? (first-char input-line) "") (non-empty-string? token)) (identify-token token) ""]
    [(string=? input-line "") ""]
    ;(
    [(and (string=? (first-char input-line) "(") (non-empty-string? token))
     (identify-token token) (identify-token (first-char input-line)) (build-tokens (rest-of-chars input-line) "")]
    [(string=? (first-char input-line) "(") (identify-token (first-char input-line)) (build-tokens (rest-of-chars input-line) "")]
    ; )
    [(and (string=? (first-char input-line) ")") (non-empty-string? token))
     (identify-token token) (identify-token (first-char input-line)) (build-tokens (rest-of-chars input-line) "")]
    [(string=? (first-char input-line) ")") (identify-token (first-char input-line)) (build-tokens (rest-of-chars input-line) "")]
    ; :=
    [(and (string=? (first-char input-line) ":=") (non-empty-string? token))
     (identify-token token) (identify-token (first-char input-line)) (build-tokens (rest-of-chars input-line) "")]
    [(string=? (first-char input-line) ":=") (identify-token (first-char input-line)) (build-tokens (rest-of-chars input-line) "")]
    ; +
    [(and (string=? (first-char input-line) "+") (non-empty-string? token))
     (identify-token token) (identify-token (first-char input-line)) (build-tokens (rest-of-chars input-line) "")]
    [(string=? (first-char input-line) "+") (identify-token (first-char input-line)) (build-tokens (rest-of-chars input-line) "")]
    ; -
    [(and (string=? (first-char input-line) "-") (non-empty-string? token))
     (identify-token token) (identify-token (first-char input-line)) (build-tokens (rest-of-chars input-line) "")]
    [(string=? (first-char input-line) "-") (identify-token (first-char input-line)) (build-tokens (rest-of-chars input-line) "")]
    ; *
    [(and (string=? (first-char input-line) "*") (non-empty-string? token))
     (identify-token token) (identify-token (first-char input-line)) (build-tokens (rest-of-chars input-line) "")]
    [(string=? (first-char input-line) "*") (identify-token (first-char input-line)) (build-tokens (rest-of-chars input-line) "")]
    ; /
    [(and (string=? (first-char input-line) "/") (non-empty-string? token))
     (identify-token token) (identify-token (first-char input-line)) (build-tokens (rest-of-chars input-line) "")]
    [(string=? (first-char input-line) "/") (identify-token (first-char input-line)) (build-tokens (rest-of-chars input-line) "")]
    ; " "
    [(and (string=? (first-char input-line) " ") (non-empty-string? token)) (identify-token token) (build-tokens (rest-of-chars input-line) "")]
    [(string=? (first-char input-line) " ") (identify-token token) (build-tokens (rest-of-chars input-line) "")]
    ; else no match, keep building
    [else
     (build-tokens (rest-of-chars input-line) (string-append token (first-char input-line)))]))


(define (identify-token token)
  ;(print token)
  (cond [(string=? token "$$") (print token)]
        [(string=? token "read") (print token)]
        [(string=? token "write") (print token)]
        [(string=? token ":=") (print token)]
        [(string=? token "+") (print token)]
        [(string=? token "-") (print token)]
        [(string=? token "*") (print token)]
        [(string=? token "/") (print token)]
        [(string=? token "(") (print token)]
        [(string=? token ")") (print token)]
        [(string=? token " ") ""]
        [(string=? token "") ""]
        [(regexp-match #rx"^[0-9]" token) (print "number")]
        [else (print "id")]))
    
; summary: Takes a file name and prints out the contents line by line,
;          including the eof itself, as well as the line number 
(define (print-file-by-line in)
  ; open input file stream
  (define input (open-input-file in #:mode 'text))
  ; define local function to grab lines until there are no more
  (define (scan input-line line-number)
    (cond
      [(eof-object? input-line) (print (string-append "Line number " (number->string line-number))) "End of file encountered"]
      [(print (string-append "Line number " (number->string line-number)))
       (build-tokens input-line "")
       (scan (read-line input) (+ 1 line-number))]))
  (scan (read-line input) 1))

; summary: Takes a list of characters
(define (build-tokens-by-line input-line-as-chars-list)
  (print input-line-as-chars-list))
  ;(build-token (string (first (input-line-as-chars-list))))
  ;(build-tokens-by-line (rest input-line-as-chars-list)))

; summary: Takes a file name and prints out the contents line by line,
;          including the eof itself, as well as the line number 
(define (print-file-by-line in)
  ; open input file stream
  (define input (open-input-file in #:mode 'text))
  ; define local function to grab lines until there are no more
  (define (iter input-line line-number)
    (cond
      [(eof-object? input-line) (print (string-append "Line number " (number->string line-number))) "End of file encountered"]
      [(build-tokens-by-line (string->list input-line))
       (print (string-append "Line number " (number->string line-number)))
       (iter (read-line input) (+ 1 line-number))]))
  (iter (read-line input) 1))

; summary: Takes a file name and prints out the contents line by line
;          as seperate strings, including the eof itself.
(define (print-file-by-line in)
  ; open input file stream
  (define input (open-input-file in #:mode 'text))
  ; define local function to grab lines until there are no more
  (define (iter input-line)
    (cond
      [(eof-object? input-line) "End of file encountered"]
      [(print input-line) (iter (read-line input))]))
  (iter (read-line input)))

; summary: Takes an open input stream and works with build-tokens to parse each chunk into
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

