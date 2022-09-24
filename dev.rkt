#lang racket
(require racket/match)

; summary: Takes an input stream and read from it until it encounte rs a whitespace or newline or eof
;          marker. Then it returns that 'token'
(define (build-token input)
  (define char (read-string 1 input))
  (define peeked (peek-string 1 0 input))
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
    
    [(string=? char " ") ""]
    [(string=? char "\n") "\n"]
    [(string-append char (build-token input))]))

; summary: Takes an input port and matches the next input to a token and returns that token.
(define (get-token in)
  (define token (build-token in))
  (print (string-append "Current token: " token))
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
    [(string=? token "") (get-token in)]
    [(string=? token " ") (get-token in)]
    [else (print (string-append "Error identifying token: " token))]))

; next-token
(define (next-token in)
  (let ([token (get-token in)])
    (cond
      [(string=? token "\n") (next-token in)]
      ; strip off newline
      [(regexp-match #rx"\n$" token)
       (substring token 0 (- (string-length token) 1))]
      ; else return token
      [else token])))

; match
(define (match input-token expected)
  (print (string-append "Matching " expected " to token " input-token))
  (cond
    [(not (string=? input-token expected)) (error (string-append "Error Matching " expected " to token " input-token))]))

; program
(define (program input-token in)
  (cond
    [(or (string=? input-token "id")
         (string=? input-token "read")
         (string=? input-token "write")
         (string=? input-token "$$"))
     (match (stmt-list input-token in) "$$") "Accept"]
    [else (print (string-append "Error in program, token: " input-token))]))

; statement list
(define (stmt-list input-token in)
  (cond
    [( or (string=? input-token "id")
          (string=? input-token "read")
          (string=? input-token "write"))
     (stmt input-token in) (stmt-list (next-token in) in)]
    [(string=? input-token "$$") input-token]
    [else (error (string-append "Error in stmt-list with token '" input-token "'"))]))

; statement
(define (stmt input-token in)
  (cond
    [(string=? input-token "id") (match input-token "id") (match (next-token in) ":=") (stmt-list (expr (next-token in) in) in)]
    [(string=? input-token "read") (match input-token "read") (match (next-token in) "id")]
    [(string=? input-token "write") (match input-token "write") (expr (next-token in) in)]
    [else (error (string-append "Error in stmt with token '" input-token "'"))]))

; expression
(define (expr input-token in)
  (cond
    [( or (string=? input-token "id")
          (string=? input-token "number")
          (string=? input-token "("))
     (term-tail (term input-token in) in)]
    [else (error (string-append "Error in expr with token '" input-token "'"))]))

; term-tail
(define (term-tail input-token in)
  (cond
    [(or (string=? input-token "+")
         (string=? input-token "-"))
     (add-op input-token in) (term-tail (term (next-token in) in) in)]
    [( or (string=? input-token ")")
          (string=? input-token "id")
          (string=? input-token "read")
          (string=? input-token "write")
          (string=? input-token "$$"))
     input-token]
    [else  (error (string-append "Error in term-tail with token '" input-token "'"))]))

; term
(define (term input-token in)
  (cond
    [( or (string=? input-token "id")
          (string=? input-token "number")
          (string=? input-token "("))
     (factor-tail (factor input-token in) in)]
    [else (error (string-append "Error in term with token '" input-token "'"))]))

; factor-tail
(define (factor-tail input-token in)
  (cond
    [(or (string=? input-token "*")
         (string=? input-token "/"))
     (mult-op input-token in) (factor (next-token in) in) (factor-tail (next-token in) in)]
    [( or (string=? input-token "+")
          (string=? input-token "-")
          (string=? input-token ")")
          (string=? input-token "id")
          (string=? input-token "read")
          (string=? input-token "write")
          (string=? input-token "$$"))
     input-token]
    [else(error (string-append "Error in factor-tail with token '" input-token "'"))]))
    
; factor
(define (factor input-token in)
  (cond
    [(string=? input-token "id") (match input-token "id")]
    [(string=? input-token "number") (match input-token "number")]
    [(string=? input-token "(") (match input-token "(") (match (expr (next-token in) in) ")")]
    [else (error (string-append "Error in factor with token '" input-token "'"))]))

; add operation
(define (add-op input-token in)
  (cond
    [(string=? input-token "+") (match input-token "+")]
    [(string=? input-token "-") (match input-token "-")]
    [else (error (string-append "Error in add-op with token '" input-token "'"))]))

; mult-op
(define (mult-op input-token in)
  (cond
    [(string=? input-token "*") (match input-token "*")]
    [(string=? input-token "/") (match input-token "/")]
    [else (error (string-append "Error in mult-op with token '" input-token "'"))]))

; parse
(define (parse input-file)
  (let ([in (open-input-file input-file #:mode 'text)])
        (print (program (next-token in) in))))

(parse "src3.txt")























; summary: main scan function
(define (scan input)
  ; open input port
  (define in (open-input-file input #:mode 'text))
  ; define local fcn
  (define (print-tokens in line-num)
    (define token (get-token in))
    (cond
      ; end of source file case
      [(or (string=? token "$$\n")
           (string=? token "$$"))
       (print "Done")]
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


;(scan "input02.txt")























(define (print-file input)
  (define char (read-string 1 input))
  (cond [(eof-object? char) "$$"]
        [else (print char)
              (print-file input)]))

;(print-file (open-input-file "src1.txt"))



































































