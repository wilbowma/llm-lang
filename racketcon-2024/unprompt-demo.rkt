#lang llm

@(require (for-syntax llm/openai/gpt4o-mini) llm/define)

@define-by-prompt![factorial]{
Define the tail recursive factorial function in Racket, named `factorial`.
}

@(unprompt (factorial 5))