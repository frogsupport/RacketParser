#lang racket

(provide (all-defined-out))

; return-char
; summary: for returning characters if we peek a terminal ahead of us.
;          This is for the case when we might have some spaces attached to a
;          terminal token such as write, and we do not want to have any
;          of those spaces attached.
(define (return-char char)
  (cond
    [(or (string=? char "")
          (string=? char " "))
     ""]

    [else
     char]))

; build-token
; summary: Takes an input stream and reads from it until it encounters a whitespace or newline or eof
;          marker. Then it returns that 'token' to get-token for identification.
(define (build-token input)
  (let ([char (read-string 1 input)])
    (let ([peeked (peek-string 1 0 input)])
      (cond
         ; reached eof without a '$$' token
        [(eof-object? char)
         (error "ERROR: EOF reached without expected token '$$'")]

        ; eof
        [(eof-object? peeked) char]
    
        ; case: :=
        [(string=? peeked ":") (return-char char)]
        [(string=? char "=") "="]
    
        ; case: (
        [(string=? peeked "(") (return-char char)]
        [(string=? char "(") "("]
    
        ; case: )
        [(string=? peeked ")") (return-char char)]
        [(string=? char ")") ")"]
    
        ; case: +
        [(string=? peeked "+") (return-char char)]
        [(string=? char "+") "+"]
    
        ; case: -
        [(string=? peeked "-") (return-char char)]
        [(string=? char "-") "-"]
    
        ; case: *
        [(string=? peeked "*") (return-char char)]
        [(string=? char "*") "*"]
    
        ; case: /
        [(string=? peeked "/") (return-char char)]
        [(string=? char "/") "/"]

        ; case whitespace or newline
        [(string=? peeked "\n") (return-char char)]
        [(string=? char " ") ""]
        [(string=? char "\n") "\n"]
        [(string-append char (build-token input))]))))

; identify-token
; summary: Takes an input stream and grabs the next 'token' from build-token.
;          Identifies the token and returns it, else if it's empty or whitespace
;          grabs another token from build-token to try again.
(define (identify-token input-token in)
  (let ([token (cons (build-token in) (cdr input-token))])
    (print (string-append "Current raw token: '" (car token) "'"))
    (cond
      ; case empty token, so grab next
      [(string=? (car token) "") (identify-token (cons "" (cdr token)) in)]
      [(string=? (car token) " ") (identify-token (cons "" (cdr token)) in)]
      
      ; if token ends in \n then it's a newline token
      ; we will use this to increment the line number
      [(regexp-match #rx"\n$" (car token))
       (cond
         [(regexp-match #rx"^[0-9]" (car token)) "number\n"]
         [(regexp-match #rx"^[a-z, A-Z]" (car token)) "id\n"]
         [(string=? "\n" (car token)) "\n"]
         
         ; special case of a single '$' ending a line
         [(string=? "$\n" (car token))
          (error (string-append "ERROR: Unable to identify '$' on line " (number->string (cdr token))))]
         
         [else
          (string-append (car token) "")])]

      ; match terminals and \n
      [(string=? (car token) "\n") "\n"]
      [(string=? (car token) "$$") "$$"]
      [(string=? (car token) "read") "read"]
      [(string=? (car token) "write") "write"]
      [(string=? (car token) ":=") ":="]
      [(string=? (car token) "+") "+"]
      [(string=? (car token) "-") "-"]
      [(string=? (car token) "*") "*"]
      [(string=? (car token) "/") "/"]
      [(string=? (car token) "(") "("]
      [(string=? (car token) ")") ")"]

      ; match id's and numbers
      [(regexp-match #rx"^[0-9]" (car token)) "number"]
      [(regexp-match #rx"^[a-z, A-Z]" (car token)) "id"]
      
      [else
       (error (string-append "ERROR: Unable to identify '" (car token) "' on line " (number->string (cdr token))))])))

; next-token
; summary: Returns the next token in an input stream. This token is a pair of the form (token, line-number).
;          Works by calling get-token, which returns a token, or a token with a newline on the end
;          which we need to output line number info.
(define (next-token input-token in)
  (let ([token (cons (identify-token input-token in) (cdr input-token))])
    (cond
      ; case "\n" recursive call because of a newline
      [(string=? (car token) "\n") (next-token (cons "" (+ (cdr token) 1)) in)]
      
      ; case token ending in "\n" strip off newline, increment line number, return token
      [(regexp-match #rx"\n$" (car token))
       (print (string-append "Returning ('" (car token) "', line " (number->string (cdr token)) ")"))
       (cons (substring (car token) 0 (- (string-length (car token)) 1)) (+ (cdr token) 1))]
      
      ; else return token
      [else
       (print (string-append "Returning ('" (car token) "', line " (number->string (cdr token)) ")"))
       token])))

