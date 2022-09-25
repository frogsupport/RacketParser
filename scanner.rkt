#lang racket

(provide (all-defined-out))

; build-token
; summary: Takes an input stream and reads from it until it encounters a whitespace or newline or eof
;          marker. Then it returns that 'token' to get-token for identification.
(define (build-token input)
  (let ([char (read-string 1 input)])
    (let ([peeked (peek-string 1 0 input)])
      (cond
        ; eof
        [(eof-object? peeked) char]
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

        ; case whitespace or newline
        [(string=? char " ") ""]
        [(string=? char "\n") "\n"]
        [(string-append char (build-token input))]))))

; identify-token
; summary: Takes an input stream and grabs the next 'token' from build-token.
;          Identifies the token and returns it, else if it's empty or whitespace
;          grabs another token from build-token to try again.
(define (identify-token in)
  (let ([token (build-token in)])
    ;(print (string-append "Current raw token: '" token "'"))
    (cond
      ; empty token, grab next
      [(string=? token "") (identify-token in)]
      [(string=? token " ") (identify-token in)]
      
      ; return a newline token, which we need for line number info
      [(regexp-match #rx"\n$" token)
       (cond
         [(regexp-match #rx"^[0-9]" token) "number\n"]
         [(regexp-match #rx"^[a-z, A-Z]" token) "id\n"]
         [(string=? "\n" token) "\n"]
         [(string-append token "")])]

      ; else match token and return
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
      
      [else
       (print (string-append "Error identifying token: " token))])))

; next-token
; summary: Returns the next token in an input stream. This token is a pair of the form (token, line-number).
;          Works by calling get-token, which returns a token, or a token with a newline on the end
;          which we need to output line number info.
(define (next-token token in)
  (let ([token-pair (cons (identify-token in) (cdr token))])
    (cond
      ; case "\n" recursive call with an empty token with an incremented line-number
      [(string=? (car token-pair) "\n") (next-token (cons "" (+ (cdr token-pair) 1)) in)]
      
      ; case token ending in "\n" strip off newline, increment line number
      [(regexp-match #rx"\n$" (car token-pair))
       (print (string-append "Returning ('" (car token-pair) "', line " (number->string (cdr token-pair)) ")"))
       (cons (substring (car token-pair) 0 (- (string-length (car token-pair)) 1)) (+ (cdr token-pair) 1))]
      
      ; else return token
      [else
       (print (string-append "Returning ('" (car token-pair) "', line " (number->string (cdr token-pair)) ")"))
       token-pair])))

