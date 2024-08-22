#lang at-exp racket/base

@(require llm llm/define (for-syntax llm/openai/gpt4o-mini) racket/list)

@define-by-prompt![draw-table #:output-file "ascii-art-table.rktd"]{
Write a Racket function `draw-table` that accepts a list of lists of strings, and renders them as an ASCII art table.
Return only a single function, with helper funtions internally defined.
Make sure all the parentheses are balanced.
}

@(display (draw-table '(("Head1" "Head2" "Header3") ("1" "23" "32123"))))
