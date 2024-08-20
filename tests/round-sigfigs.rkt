#lang llm-lang

@(require "llm-macros.rkt" racket/function)
@(require (for-syntax llm-lang/backends/gpt4o-mini))

@(current-response-timeout 300))))

@(define log10 (curryr log 10))

@define-by-prompt[round-to-n]{
Define a Racket function `round-to-n` that rounds a given number to a given number of significant digits.
}

@(displayln (round-to-n 5.0123123 3))
