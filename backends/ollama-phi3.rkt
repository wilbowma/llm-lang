#lang racket/base

(require 
 net/http-easy
 racket/port
 "config.rkt"
 "co2-base.rkt")

; https://github.com/ollama/ollama/blob/main/docs/api.md#generate-a-completion

; estimated using LLMCarbon, using data from Nvidia for hardware, and some guesses for energy efficiency and system power.
(define phi3-training-tco2
 2.71062826343271)

(define phi3-training-kwh
 (* 1000 6.776570658581774))

; guesses based on similar models; using llama 2 7B
(define phi3-model-params (model-energy-params (/ -3.89 2) (/ 31.52 2) (/ (* 4.27 (exp -2)) 2)))

; https://ourworldindata.org/grapher/carbon-intensity-electricity
(define tco2/kwh (* 170 1.102311e-6))

(define (phi3-send-prompt! prompt)
  (define rsp
   (post "http://localhost:11434/api/generate"
    #:json
    (hash 'model "phi3" 'prompt prompt 'stream #f)
    #:timeouts (make-timeout-config #:request (current-response-timeout))))
  (define response (hash-ref (response-json rsp) 'response))
  (displayln
   (model-cost-info->string (model-cost-info tco2/kwh phi3-model-params (string-length prompt) (string-length response) phi3-training-tco2 phi3-training-kwh))
   (current-cost-port))
  response)

(current-send-prompt! phi3-send-prompt!)
