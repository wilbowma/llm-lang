#lang scribble/manual
@(require
  (for-label
   racket/base
   net/http-easy
   llm
   racket/contract))

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

@defproc[(cached-send-prompt! 
          [uri string?] 
          [headers headers/c] 
          [json jsexpr?] 
          [cost-base-info model-cost-info?] 
          [inference-cost-maker (-> response? inference-cost-info?)]
          [prompt-key string?]) void?]{
A wrapper around @racket[base-send-prompt!] that uses @racket[with-cache] to cache the response.
This should be the default for new backends.
The @racket[prompt-key] should uniquely identify the prompt and its response;
you could just use the prompt text directly.
This, and other values relavant to the query, are used to compute a key into
the cache for the response.
All other parameters are the same as for @racket[base-send-prompt!].
}

@section{OpenAI}
Some @hyperlink["https://api.openai.com"]{OpenAI} models are supported in the collection @tt{llm/openai}.
Not much of the API is supported at this time.

@defmodule[llm/openai/config]
@(require (for-label llm/openai/config))

@defparam[OPENAI_API_KEY key string? #:value (getenv "OPENAI_API_KEY")]{
A parameter defining the API key used when calling the OpenAI API.
Tries to read the key from an environment variable, by default.
}

@subsection{GPT 3.5 Turbo}
@defmodule[llm/openai/gpt3-5]
@(require (for-label llm/openai/gpt3-5))

Sets the @racket[current-send-prompt!] to @racket[gpt3-5-send-prompt!] when visited.

@defproc[(gpt3-5-send-prompt! [prompt string?]) string?]{
Sends the prompt @racket[prompt] to the OpenAI API using the GPT 3.5 Turbo
model, and returns the first choice in the response messages.
}

@subsection{GPT 4o Mini}
@defmodule[llm/openai/gpt4o-mini]

@(require (for-label llm/openai/gpt4o-mini))

Sets the @racket[current-send-prompt!] to @racket[gpt4o-mini-send-prompt!] when visited.

@defproc[(gpt4-add-image! [type (or/c 'png 'jpeg)] [base64-data string?]) void?]{
Prepend an base64-encoded image, either a PNG or JPEG, to the list of images sent with the next prompt.
The prompt may refer to the image and rely on the order in which they were added.

Images can make prompts very expensive.
}

@defproc[(gpt4o-mini-send-prompt! [prompt string?]) string?]{
Sends the prompt @racket[prompt] to the OpenAI API using the GPT 4o Mini
model, and returns the first choice in the response messages.
}

@section{Ollama}
Some @hyperlink["https://ollama.com"]{Ollama} is a platform for distributing,
building, and running models locally, and several are supported in the
collection @tt{llm/ollama}.

It's API is documented at @url{https://github.com/ollama/ollama/blob/main/docs/api.md}
Not much of the API is supported at this time.

@subsection{Phi3}
@defmodule[llm/ollama/phi3]

@(require (for-label llm/ollama/phi3))

Sets the @racket[current-send-prompt!] to @racket[phi3-send-prompt!] when visited.

@defproc[(phi3-send-prompt! [prompt string?]) string?]{
Sends the prompt @racket[prompt] to the Ollama API using the Phi3 model, and
returns the response.
Assumes Ollama is running on localhost at port 11434.
}

@subsection{Llava}
@defmodule[llm/ollama/llava]

@(require (for-label llm/ollama/llava))

Sets the @racket[current-send-prompt!] to @racket[llava-send-prompt!] when visited.

@defproc[(llava-add-image! [base64-data string?]) void?]{
Prepend an base64-encoded image, either a PNG or JPEG, to the list of images sent with the next prompt.
The prompt may refer to the image and rely on the order in which they were added.
}

@defproc[(llava-send-prompt! [prompt string?]) string?]{
Sends the prompt @racket[prompt] to the Ollama API using the Llava model, and
returns the response.
Assumes Ollama is running on localhost at port 11434.
}
