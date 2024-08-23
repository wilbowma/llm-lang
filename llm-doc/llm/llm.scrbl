#lang scribble/manual
@(require
  llm
  llm/ollama/phi3
  scribble/example
  (for-label
   racket/base
   racket/port
   racket/contract
   net/http-easy
   llm
   llm/define))

@title[#:style '(toc)]{llm lang}
@author[@author+email["William J. Bowman" "wjb@williamjbowman.com"]]

@(current-response-timeout 300)

@defmodule[llm #:lang]
@racketmodname[llm] lang is a language for prompt-first software engineering.
It integrates first-class prompt-engineering directly into Racket, with
multiple backends support, a cost logging mechanism to record estimated power,
CO2, and water usage of your prompts, and automatic caching of prompt responses
to avoid unnecessarily rerunning the same prompt.

But who better to explain it than an LLM!

@nested[#:style 'inset
@prompt!{
Write a brief introduction to "LLM Lang", a programming language with first-class prompt engineering support.
Features include by-default prompt writing, escaping into the Racket programming language for computation, multiple backends, cost logging, and response caching and replay to reduce cost.
End by acknowledging that you, the LLM, wrote this introduction.
}
]

Well I'm sure that was helpful.

The collection also include libraries for accesing various LLM APIs, but is
centered on language-level integration.

To use @racketmodname[llm] lang, start your file with @tt{#lang llm}, import a backend, and start writing a prompt!
@codeblock|{
#lang llm
@(require llm/ollama/phi3)

What is 2+2?
}|

@racketmodname[llm] lang uses the at expression reader (@racket[at-exp]), so by
default you're writing a prompt, and can escape into Racket using
@code{@"@"}, such as in @code{@"@"(f)} to call the function @code{f}.
Every top-level expression---except @racket[""], @racket[(void)], and
@racket["\n"]---is collected into a prompt, which is sent either at the end of
the module, or by an explicit call to @racket[prompt!].
The result of the prompt is cached against prompt and various configuration
parameters, and will be replayed on future runs by default.
An explicit call to @racket[prompt!] allows you to capture the response, and
compute over it.
When a response is returned to the top-level, it is @racket[display]ed.

Requiring @racketmodname[llm], rather than using it as a language, allows you to
easily call prompts in your Racket programs.
@examples[
(require llm llm/ollama/phi3)
(prompt! "What is 2+2? Give only the answer without explanation.")
]

By default, the cost in terms of power, carbon, and water of each request is
estimated and logged, and reported to the @racket[current-cost-port] with
contextualizing information, to help developers understand the resource usage
of their prompts.

@examples[
(require llm llm/ollama/phi3 with-cache)
(eval:alts
(parameterize ([*use-cache?* #f])
 (prompt! "What is 2+2? Give only the answwer without explanation."))
(eval:result
"\"4\""
""
"Cumulative Query Session Costs
[0m┌───────────┬─────────────┬─────────┐
│Power (kWh)│Carbon (tCO2)│Water (L)│
├───────────┼─────────────┼─────────┤
│0          │0            │0        │
└───────────┴─────────────┴─────────┘
One-time Training Costs
[0m┌───────────┬─────────────┬─────────┐
│Power (MWh)│Carbon (tCO2)│Water (L)│
├───────────┼─────────────┼─────────┤
│6.8        │2.7          │3,700    │
└───────────┴─────────────┴─────────┘
References Resource Usage, for Context
[0m┌─────────────────────────────────┬───────┬──────────┬──────────────┐
│Reference                        │Power  │Carbon    │Water         │
├─────────────────────────────────┼───────┼──────────┼──────────────┤
│1 US Household (annual)          │10MWh  │48tCO2    │1,100L        │
├─────────────────────────────────┼───────┼──────────┼──────────────┤
│1 JFK -> LHR Flight              │       │59tCO2    │              │
├─────────────────────────────────┼───────┼──────────┼──────────────┤
│1 Avg. Natural Gas Plant (annual)│-190GWh│81,000tCO2│2,000,000,000L│
├─────────────────────────────────┼───────┼──────────┼──────────────┤
│US per capita (annual)           │77MWh  │15tCO2    │1,500,000L    │
└─────────────────────────────────┴───────┴──────────┴──────────────┘"))
]

The cost is only reported when a prompt is actually sent, and not when a cached
response to replayed.
The costs are pretty rough estimates; see @secref["LLM_Cost_Model" #:doc '(lib
"llm/llm.scrbl")] for more details.

You might use @racketmodname[llm] lang while writing your documents in
@racketmodname[scribble/base], by running a prompt to fill in some boiler plate
text:
@codeblock|{
#lang scribble/base
@(require llm llm/ollama/phi3)

@title{My cool paper}
@prompt!{
Write a concise motivation and introduction to the problem of first-class
prompt-engineering.
Make sure to use plenty of hyperbole to motivate investors to give me millions
of dollars for a solution to a non-existant problem.
}
}|

Or to help write your code in Racket:
@codeblock|{
#lang llm

@(require llm/define racket/function)
@(require (for-syntax llm/openai/gpt4o-mini))

@; I happen to know GPT4 believes this function exists.
@(define log10 (curryr log 10))

@define-by-prompt![round-to-n]{
Define a Racket function `round-to-n` that rounds a given number to a given number of significant digits.
}

@(displayln (round-to-n 5.0123123 3))
}|

By default, @racketmodname[llm] lang modifies the @racket[current-read-interaction], so you can continue talking to your LLM at the REPL:
@(require racket/runtime-path)
@(define-runtime-path screenshot "DrRacket-screenshot.png")
@image[screenshot]

@include-section{scribblings/main.scrbl}
@include-section{scribblings/define.scrbl}
@include-section{scribblings/cost.scrbl}
@include-section{scribblings/backends.scrbl}
