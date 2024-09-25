#lang llm

@(require llm/openai/gpt3-5)
@;(require llm/ollama/phi3)

You are an expert technical writer versed in communicating the story of complex technical material.
Write a brief outline for a 15 minute talk about a new programming languages, llm-lang.
The language supports first-class prompt engineering by centering writing prompts and interacting with LLMs, with escaping (unquoting) to a general-purpose programming language to enable scripting the creation of prompts, and working with the output of the LLM.
The talk should provide context for how LLMs are currently used in software engineering, and the advantages of applying language-oriented design to prompt engineering.

@(printf "~n~a~n" (prompt!))
As a technical writer, you avoid enthusiasum, and stick to factual statements supported by evidence and argument.
Do not respond to this prompt.

@(printf "~n~a~n" (prompt!))
Write presenter notes for Section 2, presenting an overview of the state-of-the-art for LLM use in software engineering.
Focus on the mode-of-use, the way software engineering interact with the LLM during software development, with specific examples, advantages, and limitations.
For example, how the chat interface compares to typical software development (writing text files), and how this may relate to development, maintenence, and usability.
Do not assume either mode is superior or inferior to the other, but provide a cold analysis. 

@(printf "~n~a~n" (prompt!))

Write slides for those presenter notes, in the Racket's Slideshow hash-lang.

@(printf "~n~a~n" (prompt!))
