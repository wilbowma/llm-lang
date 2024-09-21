#lang llm

@;(require llm/ollama/phi3)
@(require llm/config)

@(unprompt (current-send-prompt! (lambda (e history) (printf "The prompt I would send is~n~s" e))))
@(unprompt 5)
What is 1+1