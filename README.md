llm-lang
========
A language for interacting with LLMs. Inspired by GenAIScript (https://github.com/microsoft/genaiscript).

Install with `raco pkg install llm-lang` or `raco pkg install https://github.com/wilbowma/llm-lang.git#main`.

This llm-lang is LLM-first: by default, you're writing a prompt to send to a LLM.
When you want to perform normal computation, you escape into the Racket programming language.
For example, the following example asks the LLM "Are you working correctly?" and prints the reply.

```
#lang llm-lang

@(require llm-lang/backends/ollama-phi3)

Are you working correctly?
```

All top-level values are collected into a prompt and sent at the next call to `prompt!`, which returns the response from the LLM.
There is an implicit call to `prompt!` at the end of every module.

To run an llm-lang program, simply run it as a Racket program: `racket -t example1.rkt`, for example.
This will print some variation on the following:
> "Yes, I'm functioning properly. How can I assist you further? Whether it's answering questions or helping with tasks, I'm here to help!"

An explicit call to `prompt!` will let you capture the response and perform further computation.
See `example2.rkt` for a slightly more complex example.

This LLM-first approach to prompts is the main difference in design between this and GenAIScript, although llm-lang is extremely feature-poor as I wrote it in an afternoon.

The language is designed to support multiple backends, but only currently implements Phi-3 via Ollama (https://ollama.com) and GPT 3.5 (which is untested as I can't get an API key).
These backends are configuring by requiring a file that sets the right configuring variables.
Read `backends/config.rkt` and `backends/ollama-phi3.rkt` to figure out how to write new backends; it's not complicated, because the system doesn't support much yet.

I haven't thought about getting back more than text output, or configuring initial prompts, or streaming, or AICI, or pretty much anything.
I probably won't make much more progress on this without a collaborator or a fit of inspiration.
