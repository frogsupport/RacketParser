#lang racket

(require "parser.rkt")

; Enter file name of file to parse
(parse "input-files/input02.txt")

; TODO: Need to handle the newline cases more
; gracefully