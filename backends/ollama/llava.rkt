#lang racket/base

(require
 net/http-easy
 racket/port
 "../config.rkt"
 "../co2-base.rkt")

; https://github.com/ollama/ollama/blob/main/docs/api.md#generate-a-completion

;; Currently lies
(define llava-training-tco2
 2.71062826343271)

(define llava-training-kwh
 (* 1000 6.776570658581774))

; llava reports inaccurate token counts, so we're using time average with a a wild guess about my local machine's wattage
(define llava-inference-model (time-avg-inference-model (current-system-kw)))

; https://ourworldindata.org/grapher/carbon-intensity-electricity
(define tco2/kwh (* 170 1.102311e-6))

(define current-llava-images (make-parameter '()))

(provide llava-add-image!)

(define (llava-add-image! base64-data)
 (current-llava-images (cons base64-data (current-llava-images))))

(define llava-cost-info
 (model-cost-info
  'ollama/llava
  tco2/kwh
  0
  ;; using average for microsoft from li2023
  .55
  llava-training-tco2
  llava-training-kwh
  llava-inference-model))

(define (llava-send-prompt! prompt)
  (define rsp
   (post "http://localhost:11434/api/generate"
    #:json
    (hash 'model "llava" 'prompt prompt 'stream #f 'images (current-llava-images))
    #:timeouts (make-timeout-config #:request (current-response-timeout))))
  (current-llava-images '())

  (define response-hash (response-json rsp))

  (with-handlers ([values (lambda _ (displayln "Request failed; check on your LLM, it may be sad" (current-error-port)))])
   (log-model-cost!
    (cost-log-entry
     llava-cost-info
     (inference-cost-info
      (hash-ref response-hash 'prompt_eval_count)
      (hash-ref response-hash 'eval_count)
      (hash-ref response-hash 'prompt_eval_duration)
      (hash-ref response-hash 'eval_duration))))

   (hash-ref response-hash 'response)))

(current-send-prompt! llava-send-prompt!)
