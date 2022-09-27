#lang racket

(require "scanner.rkt")

(provide (all-defined-out))

; parse error
; summary: return a parse error with token and line info
(define (parse-error token)
  (error (string-append "ERROR: Unexpected token '" (car token) "' on line " (number->string (cdr token)))))

; match
; summary: Takes an input token and what it's expected to match with. If it doesn't
;          match, then it throws an error.
(define (match input-token expected)
  (print (string-append "Matching " expected " to token " (car input-token)))
  (cond
    ; token does not match expected
    [(not (string=? (car input-token) expected))
     (error (string-append "Error: Expected '" expected "', but received '" (car input-token) "' on line " (number->string (cdr input-token))))]

    ; return token if matches to preserve line number info
    [else
     input-token]))

; program
; notes: This logic flow works by passing a token around to every procedure, and whenever we match a token
; we return the next token which uses the line number from the current token. That next token is then passed
; around and matched accordingly, and if we follow the rules of the grammar from the source code we receive
; then that last token will be a '$$'. We simply match that, and return 'Accept' if all the rules are followed.
; Otherwise the parser/scanner will error out somewhere and the corresponding error will be thrown.
(define (program in line-number)
  ; initialize first token
  (let ([token (cons "" line-number)])
    ; get first token from input
    (let ([input-token (next-token token in)])
      (cond
        [(or (string=? (car input-token) "id")
             (string=? (car input-token) "read")
             (string=? (car input-token) "write")
             (string=? (car input-token) "$$"))
         (match (stmt-list input-token in) "$$") "Accept"]
    
    [else
     (parse-error input-token)]))))

; statement list
(define (stmt-list input-token in)
  (cond
    [( or (string=? (car input-token) "id")
          (string=? (car input-token) "read")
          (string=? (car input-token) "write"))
     (stmt-list (stmt input-token in) in)]

    [(string=? (car input-token) "$$") input-token]
    
    [else
     (parse-error input-token)]))

; statement
(define (stmt input-token in)
  (cond
    [(string=? (car input-token) "id") (expr (next-token (match (next-token (match input-token "id") in) ":=") in) in)]
    
    [(string=? (car input-token) "read") (next-token (match (next-token (match input-token "read") in) "id") in)]
    
    [(string=? (car input-token) "write") (expr (next-token (match input-token "write") in) in)]
    
    [else
     (parse-error input-token)]))

; expression
(define (expr input-token in)
  (cond
    [( or (string=? (car input-token) "id")
          (string=? (car input-token) "number")
          (string=? (car input-token) "("))
     (term-tail (term input-token in) in)]
    
    [else
     (parse-error input-token)]))

; term-tail
(define (term-tail input-token in)
  (cond
    [(or (string=? (car input-token) "+")
         (string=? (car input-token) "-"))
     (term-tail (term (add-op input-token in) in) in)]
    
    [( or (string=? (car input-token) ")")
          (string=? (car input-token) "id")
          (string=? (car input-token) "read")
          (string=? (car input-token) "write")
          (string=? (car input-token) "$$"))
     input-token]
    
    [else
     (parse-error input-token)]))

; term
(define (term input-token in)
  (cond
    [( or (string=? (car input-token) "id")
          (string=? (car input-token) "number")
          (string=? (car input-token) "("))
     (factor-tail (factor input-token in) in)]
    
    [else
     (parse-error input-token)]))

; factor-tail
(define (factor-tail input-token in)
  (cond
    [(or (string=? (car input-token) "*")
         (string=? (car input-token) "/"))
     (factor-tail (factor (mult-op input-token in) in) in)]
    
    [( or (string=? (car input-token) "+")
          (string=? (car input-token) "-")
          (string=? (car input-token) ")")
          (string=? (car input-token) "id")
          (string=? (car input-token) "read")
          (string=? (car input-token) "write")
          (string=? (car input-token) "$$"))
     input-token]
    
    [else
     (parse-error input-token)]))
    
; factor
(define (factor input-token in)
  (cond
    [(string=? (car input-token) "id") (next-token (match input-token "id") in)]
    
    [(string=? (car input-token) "number") (next-token (match input-token "number") in)]

    ; This is a bit cryptic if I haven't looked at it in a while because of all the nested calls, but a good description here helps to understand a lot
    ; of the logic in this file. We are trying to match the pattern '(expr)'. So in the inntermost function call
    ; we are first matching the current token to '(', then calling expression with the next token. Then we match whatever expression ends up returning
    ; to ')'. Then we return the next token up the call chain.
    [(string=? (car input-token) "(") (next-token (match (expr (next-token (match input-token "(") in) in) ")") in)]
    
    [else
     (parse-error input-token)]))

; add operation
(define (add-op input-token in)
  (cond
    [(string=? (car input-token) "+") (next-token (match input-token "+") in)]
    
    [(string=? (car input-token) "-") (next-token (match input-token "-") in)]
    
    [else
     (parse-error input-token)]))

; mult-op
(define (mult-op input-token in)
  (cond
    [(string=? (car input-token) "*") (next-token (match input-token "*") in)]
    
    [(string=? (car input-token) "/") (next-token (match input-token "/") in)]
    
    [else
     (parse-error input-token)]))

; parse
(define (parse input-file)
  (cond
    ; check if file exists
    [(not (file-exists? input-file)) (error (string-append "ERROR: File '" input-file "' not found :("))]

    ; run program
    [else
     (let ([in (open-input-file input-file #:mode 'text)])
       (let ([line-number 1])
         ; will print "Accept" if syntactically correct, else error out in program
         (print (program in line-number))
         (close-input-port in)))]))
  
