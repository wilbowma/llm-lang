#lang racket/base

(require
 net/http-easy
 racket/port
 "config.rkt"
 "../base.rkt"
 "../config.rkt"
 "../cost-base.rkt")

(provide OPENAI_API_KEY)

; Using LLMCarbon's number for GPT 3, which is validated, but is probably an underestimate of the training cost of 3.5-Turbo.
(define gpt3-training-tco2
 553.3446700507615)

(define gpt3-training-kwh
 (* 1000 1289.8477157360408))

; Using Llama-2 13b as estimate. The parameter count of GPT3.5 Turbo, isn't public, but it's guessed to be 20B.
; But, it's also apparently heavily optimized. Better to underestimate, I guess?
;
; https://x.com/labenz/status/1654853390839472134/photo/1
; https://github.com/llv22/gpt4_essay/blob/master/GPT-4-9.JPG
; https://www.reddit.com/r/LocalLLaMA/comments/17lvquz/comment/k7hjc8b/
;
; This seems to line up with estimates of energy usage of GPT 3 from brown2020a, which estimated .004kwh usage per request.
; I'm getting .002 for small examples, so if we assume some constant overhead and/or optimization, or the input tokens matter, this seems to be validated.
(define gpt3-inference-model (wilkins-inference-model -6.79 56.01 (* 7.29 (exp -2))))

; https://ourworldindata.org/grapher/carbon-intensity-electricity
(define tco2/kwh (* 170 1.102311e-6))

;; using US average of Microsoft's servers from li2023
(define L/kWh .55)

(define gpt3-cost-info
 (model-cost-info 'gpt3.5-turbo tco2/kwh L/kWh L/kWh gpt3-training-tco2 gpt3-training-kwh gpt3-inference-model))

(provide gpt3-5-send-prompt!)

(define (gpt3-5-send-prompt! prompt)
  (define response-hash
   (cached-send-prompt!
    "https://api.openai.com/v1/chat/completions"
    (hasheq 'Authorization (format "Bearer ~a" (OPENAI_API_KEY)))
    (hasheq 'model "gpt-3.5-turbo"
            'messages (list (hash 'role "user" 'content prompt 'stream #f)))

    gpt3-cost-info
    (lambda (response-hash)
     (define usage (hash-ref response-hash 'usage))
     (inference-cost-info
      (hash-ref usage 'prompt_tokens)
      (hash-ref usage 'completion_tokens)
      #f
      #f))
    prompt))

  (hash-ref (hash-ref (list-ref (hash-ref response-hash 'choices) 0) 'message) 'content))

(current-send-prompt! gpt3-5-send-prompt!)