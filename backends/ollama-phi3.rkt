#lang racket/base

(require 
 net/http-easy
 racket/port
 "config.rkt"
 "co2-base.rkt")

; https://github.com/ollama/ollama/blob/main/docs/api.md#generate-a-completion

; estimated using LLMCarbon, using some guesses for energy efficiency
(define phi3-training-co2
 2.71062826343271)

(define phi3-training-mwh
 6.776570658581774)

; guesses based on similar models; using llama 2 7B
(define phi3-model-params (model-energy-params (/ -3.89 2) (/ 31.52 2) (/ (* 4.27 (exp -2)) 2)

(define (phi3-send-prompt! prompt)
  (define rsp
   (post "http://localhost:11434/api/generate"
    #:json
    (hash 'model "phi3" 'prompt prompt 'stream #f)
    #:timeouts (make-timeout-config #:request (current-response-timeout))))
  (displayln-co2-info
    (make-co2-info (token-length prompt) (token-length response) phi3-training-co2))
  (hash-ref (response-json rsp) 'response))

(current-send-prompt! phi3-send-prompt!)
