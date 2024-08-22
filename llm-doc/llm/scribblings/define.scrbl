#lang scribble/manual
@(require 
  scribble/examples
  (for-label
   racket/base))

@title{LLM Compile-time Metaprogramming}
@defmodule[llm/define]

@defform*[((define-by-prompt! id strs ...)
           (define-by-prompt! id #:output-file file-str strs ...)
           #:contracts ([id identifier?] [str string?])
           )]{
Defines the @racket[id] by requesting the Racket definition of this value using
the prompt @racket[strs].
The prompt is sent during phase 1, and the response is converted to syntax is
using @racket[datum->syntax] and the lexical context information of the input
to this form, and used to generate a definition of the identifier @racket[id].
The prompt must be string literal.

Optionally specifies an output file @racket[file-str], as a string-path literal, to write the definition to.
This is useful if you may need to modify the result.
This file provides a second level of caching, if the backend is already caching responses.

Requires a backend imported @racket[for-syntax]

NOTE: This will compile unknown code, from the internet, directly into program,
leaving no trace of an LLM invokation at run time.
This is not advisable.
You should probably sandbox the result or something.

@examples[
(require llm llm/define (for-syntax llm/ollama/phi3))

(define-by-prompt! my-identity "Define the Racket function `my-identity`, which simply returns its input")
(my-identity 5)
]
}


