#lang llm-lang

@(require llm-lang/backends/ollama-phi3 racket/file racket/system)

@(define tmpd (make-temporary-directory))
@(current-directory tmpd)
@(void (system "git clone https://github.com/racket/racket.git"))

Please write a change log for all changes between versions 8.9 and the 8.10 for Racket.
The complete commit history is given below:
```
@(system "cd racket; git --no-pager log")
```

@(delete-directory tmpd)
