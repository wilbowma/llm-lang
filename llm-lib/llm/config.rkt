#lang racket/base

(require racket/contract racket/string)

(provide (all-defined-out))

(define current-prompt-port (make-parameter (open-output-string)))

(define current-llm-backend-name (make-parameter 'undefined))

(define current-send-prompt!
 (make-parameter
  (lambda (e) (error "Unconfigured backend prompt sender; please require a backend. Failed to send prompt" e))))

; expects list of hasheq with 'role either "user" or "assistant" and 'content with the prompt or response.
; in reverse chronological order
(define current-chat-history (make-parameter '()))

(define (append-prompt! str)
 (void (fprintf (current-prompt-port) "~a" str)))

; role either 'user or 'assistant
(define (append-message! role content)
 (current-chat-history (cons (hasheq 'role (symbol->string role) 'content content) (current-chat-history))))

(define (prompt! . strs)
 (for ([str strs])
  (append-prompt! str))
 (define prompt (string-trim (bytes->string/utf-8 (get-output-bytes (current-prompt-port) #t) #\uFFFD) #:repeat? #t))
 (if (equal? "" prompt)
     (void)
     ((current-send-prompt!) prompt (current-chat-history))))

(define current-response-timeout
 (make-parameter 120 (lambda (e) (and (natural-number/c e) e))))

(define-logger llm-lang)
