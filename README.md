llm-lang
========

## Getting Started
Install with `raco pkg install llm` or `raco pkg install https://github.com/wilbowma/llm-lang.git?path=llm#main`.
Then open the docs, `raco doc llm`, to see more! Or, browse them online: https://docs.racket-lang.org/llm/index.html

## TLDR
llm-lang is a language for first-class prompt-engineering in Racket. Inspired by GenAIScript (https://github.com/microsoft/genaiscript), but designed and implemented with language-oriented design in Racket. Everything is prompt-first, so all text in files in the REPL is, by default, unstructured text added to a prompt. You must escape the prompt to do regular programming, generate prompts, splice values into prompts, etc. The language is also designed to pay attention to, and give the programming controls over, the *cost* of prompts: https://docs.racket-lang.org/llm/LLM_Cost_Model.html.

![](screenshot.png)

See [demo.mp4](demo.mp4) for an out-dated usage demo video.

The language is designed to support multiple backends; see https://docs.racket-lang.org/llm/LLM_Backends.html for the current list of supported baceknds.

