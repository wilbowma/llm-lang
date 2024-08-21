#lang scribble/manual
@(require 
  scribble/examples
  (for-label 
   llm 
   racket/base
   racket/port
   racket/contract))

@title{LLM API Library}
@;defmodule[llm #:no-declare]

@defproc[(prompt! [strs string?] ...) void?]{
Sends the current prompt, from @racket[current-prompt-port], to the current LLM backend via @racket[current-send-prompt!].
The values @racket[strs] are first written to @racket[current-prompt-port].

@examples[
(require llm llm/ollama/phi3)
(display
 (prompt!
  "Please write a haiku about the reliability and performance of Phi3 for use in software engineering."
  "Make it a short haiku."))
]
}

@defparam[current-prompt-port port string-port?]{
A parameter to which the prompt is written before being sent to the current backend.
Default value is a new output @racket[string-port?].
}

@defparam[current-send-prompt! prompt! (-> string? ... void?)]{
A parameter that defines how to send a prompt to the current backend.
Typically configured by importing a backend, rather than accessed manually.
}

@defparam[current-response-timeout seconds natural-number/c]{
A parameter that defines the how many seconds to wait for a response from the LLM after sending a prompt.
}


