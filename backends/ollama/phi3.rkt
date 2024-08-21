#lang racket/base

(require
 net/http-easy
 racket/port
 racket/function
 "../base.rkt"
 "../config.rkt"
 "../cost-base.rkt")

; https://github.com/ollama/ollama/blob/main/docs/api.md#generate-a-completion

; estimated using LLMCarbon, using data from Nvidia for hardware, and some guesses for energy efficiency and system power.
(define phi3-training-tco2
 2.71062826343271)

(define phi3-training-kwh
 (* 1000 6.776570658581774))

; guesses based on similar models; using llama 2 7B
; Validation data: benchmarks suggest my iMac 27" 2017 3.5 i5 16 GB is drawing 1KW, which suggest the model is closer to 2x reality.
(define phi3-inference-model (wilkins-inference-model (/ -3.89 2) (/ 31.52 2) (/ (* 4.27 (exp -2)) 2)))

; https://ourworldindata.org/grapher/carbon-intensity-electricity
(define tco2/kwh (* 170 1.102311e-6))

;; using average for microsoft from li2023
;; 0 for local costs
(define phi3-cost-info
 (model-cost-info 'ollama/phi3 tco2/kwh 0 .55 phi3-training-tco2 phi3-training-kwh phi3-inference-model))

(define (phi3-send-prompt! prompt)
 (define response-hash
  (cached-send-prompt!
   "http://localhost:11434/api/generate"
   (hasheq)
   (hasheq 'model "phi3" 'prompt prompt 'stream #f)
   phi3-cost-info
   (lambda (response-hash)
    (inference-cost-info
     (hash-ref response-hash 'prompt_eval_count)
     (hash-ref response-hash 'eval_count)
     (hash-ref response-hash 'prompt_eval_duration)
     (hash-ref response-hash 'eval_duration)))
   prompt))

  (hash-ref response-hash 'response))

(current-send-prompt! phi3-send-prompt!)
