#lang racket

(require "scanner.rkt")

(provide (all-defined-out))

; match
; summary: Takes an input token and what it's expected to match with. If it doesn't
;          match, then it throws an error.
(define (match input-token expected)
  (print (string-append "Matching " expected " to token " (car input-token)))
  (cond
    [(not (string=? (car input-token) expected)) (error (string-append "Error Matching " expected " to token " (car input-token) " on line " (cdr input-token)))]
    [else
     input-token]))

; program
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
         (let ([last-token (stmt-list input-token in)])
           (match last-token "$$") (cons "Accept" (cdr last-token)))]
    
    [else
     (error (string-append "Error in program with token '" (car input-token) "' on line " (number->string (cdr input-token))))]))))

; statement list
(define (stmt-list input-token in)
  (cond
    [( or (string=? (car input-token) "id")
          (string=? (car input-token) "read")
          (string=? (car input-token) "write"))
     (stmt-list (stmt input-token in) in)]

    [(string=? (car input-token) "$$") input-token]
    
    [else
     (error (string-append "Error in stmt-list with token '" (car input-token) "' on line " (cdr input-token)))]))

; statement
(define (stmt input-token in)
  (cond
    [(string=? (car input-token) "id") (match input-token "id") (match (next-token in) ":=") (expr (next-token in) in)]
    
    [(string=? (car input-token) "read") (next-token (match (next-token (match input-token "read") in) "id") in)]
    
    [(string=? (car input-token) "write") (match input-token "write") (expr (next-token in) in)]
    
    [else
     (error (string-append "Error in stmt with token '" (car input-token) "' on line " (cdr input-token)))]))

; expression
(define (expr input-token in)
  (cond
    [( or (string=? (car input-token) "id")
          (string=? (car input-token) "number")
          (string=? (car input-token) "("))
     (term-tail (term input-token in) in)]
    
    [else
     (error (string-append "Error in expr with token '" (car input-token) "' on line " (cdr input-token)))]))

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
     (error (string-append "Error in term-tail with token '" (car input-token) "' on line " (cdr input-token)))]))

; term
(define (term input-token in)
  (cond
    [( or (string=? (car input-token) "id")
          (string=? (car input-token) "number")
          (string=? (car input-token) "("))
     (factor-tail (factor input-token in) in)]
    
    [else
     (error (string-append "Error in term with token '" (car input-token) "' on line " (cdr input-token)))]))

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
     (error (string-append "Error in factor-tail with token '" (car input-token) "' on line " (cdr input-token) ))]))
    
; factor
(define (factor input-token in)
  (cond
    [(string=? (car input-token) "id") (match input-token "id") (next-token in)]
    
    [(string=? (car input-token) "number") (match input-token "number") (next-token in)]
    
    [(string=? (car input-token) "(") (match input-token "(") (match (expr (next-token in) in) ")") (next-token in)]
    
    [else
     (error (string-append "Error in factor with token '" (car input-token) "' on line " (cdr input-token)))]))

; add operation
(define (add-op input-token in)
  (cond
    [(string=? (car input-token) "+") (match input-token "+") (next-token in)]
    
    [(string=? (car input-token) "-") (match input-token "-") (next-token in)]
    
    [else
     (error (string-append "Error in add-op with token '" (car input-token) "' on line " (cdr input-token)))]))

; mult-op
(define (mult-op input-token in)
  (cond
    [(string=? (car input-token) "*") (match input-token "*") (next-token in)]
    
    [(string=? (car input-token) "/") (match input-token "/") (next-token in)]
    
    [else
     (error (string-append "Error in mult-op with token '" (car input-token) "' on line " (cdr input-token)))]))

; parse
(define (parse input-file)
  (cond
    ; check if file exists
    [(not (file-exists? input-file)) (error (string-append "Error: File '" input-file "' does not exist :("))]

    ; run program
    [else
     (let ([in (open-input-file input-file #:mode 'text)])
       (let ([line-number 1])
         (print (car (program in line-number)))
         (close-input-port in)))]))
  
