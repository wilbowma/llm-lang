#lang llm

@(require
  llm/openai/gpt4o-mini
  racket/file
  net/base64
  racket/port
  racket/runtime-path
  racket/path
  racket/dict
  racket/pretty
  pict)

@(current-response-timeout (* 10 60))

@(define-runtime-path image-dir "images")

@(current-print (lambda (x) (if (void? x) (void) (pretty-print x))))
@(unprompt
  (for/fold ([images '()])
            ([file (in-directory image-dir)]
             #:when (path-has-extension? file ".png"))
    (gpt4-add-image! 'png (bytes->string/utf-8 (base64-encode (port->bytes (open-input-file file) #:close? #t) "")))
    (dict-set images (bitmap file) (prompt! "Please provide alt text for the following image."))))
