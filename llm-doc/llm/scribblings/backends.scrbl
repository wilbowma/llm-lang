#lang scribble/manual
@(require
  (for-label
   racket/base
   net/http-easy))

@title{LLM Backends}
@defmodule[llm/base]

Internal tools for developing new backends.

@defproc[(base-send-prompt! [uri string?] [headers headers/c] [json jsexpr?] [cost-base-info model-cost-info?] [inference-cost-maker (-> response? inference-cost-info?)]) void?]{
A wrapper around @racket[post] used to send a prompt to the backend at
@racket[uri], using the provided @racket[headers] and @racket[json] payload.

The wrapper records some debug and cost logging information.
@racket[inference-cost-maker] receives the response, and should build an
@racket[inference-cost-info] for use in the @racket[cost-log-entry] for this
prompt.
}

@section{OpenAI}
@section{Ollama}
