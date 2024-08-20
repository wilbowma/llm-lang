#lang racket/base

(require racket/contract)

(provide (all-defined-out))

(define current-prompt-port (make-parameter (open-output-string)))

(define current-llm-backend-name (make-parameter 'undefined))

(define current-send-prompt!
 (make-parameter
  (lambda (e) (error "Unconfigured backend prompt sender; please require a backend. Failed to send prompt" e))))

(define current-response-timeout
 (make-parameter 120 (lambda (e) (and (natural-number/c e) e))))
