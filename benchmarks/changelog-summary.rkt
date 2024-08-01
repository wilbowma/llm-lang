#lang llm-lang

@(require
  llm-lang/backends/ollama-phi3
  racket/file
  racket/system
  racket/port)

@(current-directory "/tmp/")
@(void (system "git clone https://github.com/racket/racket.git || true"))

; 10 minutes
@(current-response-timeout (* 10 60))

Please write a change log for all changes between versions 8.9 and the 8.10 for Racket.
The complete commit history is given below:
```
@(with-output-to-string (lambda () (system "cd racket; git --no-pager log")))
```

@(displayln (prompt!))
