#lang racket

; summary: Takes a source program file and converts it into a token stream
; for the parser
; notes: A token is the smallest unit of code handled by a parser. A token can
; refer to a literal string, a value, or a class of things.
;
; param: lexer-src-loc - The program source file
; returns: The token stream list
(define lexer lexer-src-loc)
   






; summary: Takes a program source file and analyzes it for syntactical correctness.
; If the file is syntactically correct, return true. Otherwise print the offending line
; and it's line number and false.
; notes:
; param: src-file - The program source file
; returns: True if correct, false if not and offending line and number
(define parse src-file)




(define (selectionSort arr)
  (cond [(empty? arr) '()]
        [else (define x0 (apply min arr))
              (cons x0 (SelectionSort (remove x0 arr)))])) 

(define myList '(4 2 3 1 20 77 99 33 47 92 74 0 3 6))

print(SelectionSort myList)

; 'provide' makes functions available outside the file. Definitions are private by default in racket.
(provide parse)
(provide lexer)

; summary: 
; notes:
; param: 
; returns: