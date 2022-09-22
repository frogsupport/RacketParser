#lang racket
(require racket/match)

; summary: Takes an input stream and read from it until it encounte rs a whitespace or newline or eof
;          marker. Then it returns that 'token'
(define (build-token input)
  (define char (read-string 1 input))
  (define peeked (peek-string 1 0 input))
  (cond
    ; if we reach eof without a $$ then it's not a valid program
    [(eof-object? peeked) ""]
    [(eof-object? char) ""]
    
    ; case: :=
    [(string=? peeked ":") char]
    ;[(string=? char ":") ":"]
    [(string=? char "=") "="]
    
    ; case: (
    [(string=? peeked "(") (if (string=? char " ") "" char)]
    [(string=? char "(") "("]
    
    ; case: )
    [(string=? peeked ")") char]
    [(string=? char ")") ")"]
    
    ; case: +
    [(string=? peeked "+") (if (string=? char " ") "" char)]
    [(string=? char "+") "+"]
    
    ; case: -
    [(string=? peeked "-") char]
    [(string=? char "-") "-"]
    
    ; case: *
    [(string=? peeked "*") char]
    [(string=? char "*") "*"]
    
    ; case: /
    [(string=? peeked "/") char]
    [(string=? char "/") "/"]
    
    [(string=? char " ") ""]
    [(string=? char "\n") "\n"]
    [(string-append char (build-token input))]))

; summary: Takes an input port and matches the next input to a token and returns that token.
(define (get-token in)
  (define token (build-token in))
  ;(print (string-append "Current token: " token))
  (cond
    ; return a newline token, which we need for line number
    [(regexp-match #rx"\n$" token)
     (cond
       [(regexp-match #rx"^[0-9]" token) "number\n"]
       [(regexp-match #rx"^[a-z, A-Z]" token) "id\n"]
       [(string=? "\n" token) "\n"]
       [(string-append token "")])]
    [(string=? "\n" token) "\n"]
    [(string=? token "$$") "$$"]
    [(string=? token "read") token]
    [(string=? token "write") token]
    [(string=? token ":=") token]
    [(string=? token "+") token]
    [(string=? token "-") token]
    [(string=? token "*") token]
    [(string=? token "/") token]
    [(string=? token "(") token]
    [(string=? token ")") token]
    [(regexp-match #rx"^[0-9]" token) "number"]
    [(regexp-match #rx"^[a-z, A-Z]" token) "id"]
    [else "Grab-Next-Token"]))

; summary: main parse function
(define (parse input)
  ; open input port
  (define in (open-input-file input #:mode 'text))
  ; define local fcn
  (define (print-tokens in line-num)
    (define token (get-token in))
    (cond
      ; end of source file case
      [(string=? token "$$\n") (print "Done")]
      ; empty token case
      [(string=? token "Grab-Next-Token") (print-tokens in line-num)]
      [(string=? token "\n")
       (print (string-append "Line number " (number->string (+ line-num 1))))
       (print-tokens in (+ line-num 1))]
      ; increment line number case
      [(regexp-match #rx"\n$" token)
       (print (substring token 0 (- (string-length token) 1)))
       (print (string-append "Line number " (number->string (+ line-num 1))))
       (print-tokens in (+ line-num 1))]
      ; recursive case
      [else (print token) (print-tokens in line-num)]))
  (print "Line number 1")
  (print-tokens in 1))

(parse "input04.txt")























(define (print-file input)
  (define char (read-string 1 input))
  (cond [(eof-object? char) "$$"]
        [else (print char)
              (print-file input)]))

;(print-file (open-input-file "src1.txt"))



































































