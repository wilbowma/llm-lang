#lang racket/base

(require 
 net/http-easy
 racket/port
 "config.rkt")

(define OPENAI_API_KEY (make-parameter #f))

(define (gpt3-5-send-prompt! prompt)
  (define rsp
   (post "https://api.openai.com/v1/chat/completions"
    #:headers
    (hash 'Authorization (format "Bearer ~a" (OPENAI_API_KEY)))
    #:json
    (hash 'model "gpt-3.5-turbo" 
          'messages (hash 'role "user" 'content prompt 'stream #f))
    #:timeouts (make-timeout-config #:request 120)))
  (hash-ref (hash-ref (list-ref (hash-ref (response-json rsp) 'choices) 0) 'message) 'content))

(current-send-prompt! gpt3-5-send-prompt!)
