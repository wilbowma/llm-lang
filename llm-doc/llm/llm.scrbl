#lang scribble/manual
@(require llm llm/ollama/phi3)

@title[#:style '(toc)]{LLM Lang}
@author[@author+email["William J. Bowman" "wjb@williamjbowman.com"]]

@(current-response-timeout 300)

@defmodule[llm #:lang]
@prompt!{
Write a brief introduction to "LLM Lang", a programming language with first-class prompt engineering support.
Features include by-default prompt writing, escaping into the Racket programming language for computation, multiple backends, cost logging, and response caching and replay to reduce cost. 
End by acknowledging that you, the LLM, wrote this introduction.
}

Well I'm sure that was helpful.
LLM lang (@racketmodname[llm]) is a language for prompt-first software engineering.
It integrates prompt-engineering directly into Racket, with multiple backends
support, a cost logging mechanism to record estimated power and CO2 usage of
your prompts, and automatic caching of prompt responses to avoid unnecessarily
rerunning the same prompt.

The collection also include libraries for accesing various LLM APIs, but is
centered on language-level integration.
