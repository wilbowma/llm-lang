#lang llm

@(require llm llm/define racket/function)
@(require (for-syntax llm/openai/gpt4o-mini))

@; I happen to know GPT4 believes these functions exists.
@(define log10 (curryr log 10))
@(define ceil ceiling)

@define-by-prompt![round-to-n]{
Define a Racket function `round-to-n` that rounds a given number to a given number of significant digits.
}

@(displayln (round-to-n 5.0123123 3))
