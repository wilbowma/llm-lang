#lang llm-lang

@(require
  llm-lang/ollama/llava
  racket/file
  net/base64
  racket/port
  racket/runtime-path
  racket/path)

@(current-response-timeout (* 10 60))

@(define-runtime-path image-dir "images")

@(for ([file (in-directory image-dir)]
       #:when (path-has-extension? file ".png"))
  (llava-add-image! (bytes->string/utf-8 (base64-encode (port->bytes (open-input-file file) #:close? #t) "")))
  (displayln (prompt! "Please provide alt text for the following image.")))
