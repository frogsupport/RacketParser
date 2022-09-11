#lang racket
(require racket/match)

(define (build-token input)
  (define char (read-string 1 input))
  (cond [(eof-object? char) "$$"]
        [(string=? char " ") ""]
        [(string=? char "\n") ""]
        [(string-append char (build-token input))]))

; summary: Takes an input stream and works with build-tokens to parse each chunk into
;          its proper identifier. For example anything that is a number is simply
;          converted into a "number", and anything that is an identifier into "id".
(define (next-token in)
  (define token (build-token in))
  (cond [(string=? token "$$") token]
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
        [else "id"]))

(define (program inputToken)
  (cond [(string=? inputToken "id" "read" "write" "$$") #t]
        [else (print "Parse error on line")]))




(define (parse input)
  (define in (open-input-file input #:mode 'text))
  ;(define inputToken (next-token in))
  (define lineNumber 1)
  (define line (read-line in))
  (print (string-split line))
  ;(program inputToken)
  (close-input-port in))




; open input stream
;(define in (open-input-file "src1.txt" #:mode 'text))

(parse "src1.txt")

; close input stream
;(close-input-port in)

; summary: 
; notes:
; param: 
; returns: