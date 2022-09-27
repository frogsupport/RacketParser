#lang racket

(require "parser.rkt")

; Enter file name of file to parse
(parse "input03.txt")

; TODO: Getting unexpected behavior when ending lines with things such as 'write'. This get
; put into an id token when it should be write, need to handle the newline cases more
; gracefully