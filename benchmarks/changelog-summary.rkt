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

Please write a change log for release of the new v8.10 versions v8.9 and the v8.10 for Racket.
The summary should include major changes since version v8.9, and be accessible to most Racket users and developers, informing them of major bug fixes, features, and feature changes.
The complete git history for all commits between these two versions is given below:
```
@(with-output-to-string (lambda () (system "cd racket; git --no-pager log v8.9..v8.10")))
```

@(displayln (prompt!))
