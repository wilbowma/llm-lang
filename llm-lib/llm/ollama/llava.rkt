#lang racket/base

(require
 net/http-easy
 racket/port
 "../base.rkt"
 "../config.rkt"
 "../cost-base.rkt")

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

(provide llava-send-prompt!)

(define (llava-send-prompt! prompt [messages '()])
 (define new-messages
  (reverse
   (cons (hasheq 'role "user" 'content prompt) messages)))
 (define response-hash
  (cached-send-prompt!
   "http://localhost:11434/api/chat"
   (hasheq)
   (hash 'model "llava" 'messages new-messages 'stream #f 'images (current-llava-images))
   llava-cost-info
   (lambda (response-hash)
    (inference-cost-info
     (hash-ref response-hash 'prompt_eval_count)
     (hash-ref response-hash 'eval_count)
     (hash-ref response-hash 'prompt_eval_duration)
     (hash-ref response-hash 'eval_duration)))
   prompt
   messages))

  (current-llava-images '())
  (let ([resp (hash-ref (hash-ref response-hash 'message) 'content)])
   (append-prompt! 'assistant resp)
   resp))

(current-send-prompt! llava-send-prompt!)
