#lang racket/base

(require 
 net/http-easy
 racket/port
 "config.rkt")

; https://github.com/ollama/ollama/blob/main/docs/api.md#generate-a-completion

(define (phi3-send-prompt! prompt)
  (define rsp
   (post "http://localhost:11434/api/generate"
    #:json
    (hash 'model "phi3" 'prompt prompt 'stream #f)
    #:timeouts (make-timeout-config #:request (current-response-timeout))))
  (hash-ref (response-json rsp) 'response))

(current-send-prompt! phi3-send-prompt!)
