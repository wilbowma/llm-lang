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
   llm/define
   with-cache))

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

@racketmodname[llm] lang uses the at expression reader (@racketmodname[at-exp]), so by
default you're writing a prompt, and can escape into Racket using
@code{@"@"}, such as in @code{@"@"(f)} to call the function @code{f}.
Every top-level expression---except @racket[""], @racket[(void)], and
explicitly @racket[unprompt]ed values---is collected into a prompt, which is
sent either at the end of the module, or by an explicit call to
@racket[prompt!].
The result of the prompt is cached against prompt and various configuration
parameters, and will be replayed on future runs by default.
An explicit call to @racket[prompt!] allows you to capture the response, and
compute over it.

To return a value to Racket, instead of to the prompt, use the keyword @racket[unprompt].
@codeblock|{
#lang llm
@(require llm/ollama/phi3)

@(unprompt 5)
}|

This example returns @racket[5] to Racket, which is printed using the @racket[current-print] handler.
@racket[unprompt] is roughly analogous to @racket[unquote], and by default, all
top-level values are under an implicit quasiquote operation to build the
prompt.

@nested[#:style 'inset
@defform[(unprompt e)]{
Returns @racket[e] as a value for interpretation by the current continuation, instead of collecting it in a prompt.
The value may still be collected in a prompt, if the current continuation is
explicitly constructing a prompt, such as @racket[(prompt! (format "~a"
(unprompt 5)))].
}
]

The @racketmodname[at-exp] reader is also used in the REPL, and the @racket[unprompt] form is also recognized there.
Multiple unstructured datums can be written in the REPL, which are collected into a prompt.
For example, entering @tt{What is 2+2?} in @racketmodname[llm] lang REPL will send the prompt @racket["What is 2+2?"].
Any @racket[unprompt]ed values in the REPL are returned as multiple return values, and displayed using the @racket[current-print] handler.
For example, entering @tt|{@(unprompt 5) @(unprompt 6)}| at the REPL returns the values @racket[(values 5 6)] to the REPL.


@racketmodname[llm] lang redefines the @racket[current-print] handler @racket[display] all values, except @racket[(void)].
Since the primary mode of interaction is a dialogue with an LLM, this makes reading the response easier.

Requiring @racketmodname[llm], rather than using it as a language, allows you to
easily call prompts in your Racket programs.
@examples[
(require llm llm/ollama/phi3)
(prompt! "What is 2+2? Give only the answer without explanation.")
]

By default, the resource cost in terms of power, carbon, and water of each
request is estimated and logged, and reported to the @racket[logger]
@racket[llm-lang-logger] with contextualizing information, to help developers
understand the resource usage of their prompts.
This data, in raw form, is also available through
@racket[current-carbon-use], @racket[current-power-use], and
@racket[current-water-use].

@examples[
(require llm llm/ollama/phi3 with-cache)
(eval:alts
(parameterize ([*use-cache?* #f])
 (with-logging-to-port (current-error-port)
  (lambda () (prompt! "What is 2+2? Give only the answwer without explanation."))
  #:logger llm-lang-logger 'info 'llm-lang))
(eval:result
"\"4\""
""
"llm-lang: Cumulative Query Session Costs
┌──────────┬─────────────┬─────────┐
│Power (Wh)│Carbon (gCO2)│Water (L)│
├──────────┼─────────────┼─────────┤
│0.064     │0.012        │0        │
└──────────┴─────────────┴─────────┘
One-time Training Costs
┌───────────┬─────────────┬─────────┐
│Power (MWh)│Carbon (tCO2)│Water (L)│
├───────────┼─────────────┼─────────┤
│6.8        │2.7          │3,700    │
└───────────┴─────────────┴─────────┘
References Resource Usage, for Context
┌─────────────────────────────────┬───────┬──────────┬──────────────┐
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

The cost is logged each time a prompt is actually sent, and not when a cached
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
