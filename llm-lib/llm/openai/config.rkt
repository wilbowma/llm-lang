#lang racket/base

(provide OPENAI_API_KEY)

(define OPENAI_API_KEY
  (make-parameter (getenv "OPENAI_API_KEY")
    (lambda (e)
      (unless (string? e)
       (error "OPENAI_API_KEY looks invalid; should be a string"))
      e)))
