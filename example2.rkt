#lang llm-lang

@(require llm-lang/backends/ollama-phi3 racket/port)

I am a scientist interested in consturctive, critical feedback. I value precision, brevity, and falsifiability.

Please provide feedback on the following research submission:

@(port->string (open-input-file "test.md"))

@(displayln (prompt!))
