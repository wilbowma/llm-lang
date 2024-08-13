#lang racket/base

(require
 net/http-easy
 racket/port
 "config.rkt"
 "co2-base.rkt")

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
; This seems to line up with estimates of energy usage of GPT 3 from brown2020a, which showed .004kwh usage per page generated.
; I'm getting .002 for small examples, so if we assume some constant overhead and/or optimization, or the input tokens matter, this seems to be validated.
(define gpt3-inference-model (wilkins-inference-model -6.79 56.01 (* 7.29 (exp -2))))

; https://ourworldindata.org/grapher/carbon-intensity-electricity
(define tco2/kwh (* 170 1.102311e-6))

(define gpt3-cost-info
 (model-cost-info 'gpt3.5-turbo tco2/kwh gpt3-training-tco2 gpt3-training-kwh gpt3-inference-model))

(define OPENAI_API_KEY
  (make-parameter #f
    (lambda (e)
      (unless (string? e)
       (error "OPENAI_API_KEY looks invalid; should be a string"))
      e)))

(define (gpt3-5-send-prompt! prompt)
  (define rsp
   (post "https://api.openai.com/v1/chat/completions"
    #:headers
    (hasheq 'Authorization (format "Bearer ~a" (OPENAI_API_KEY)))
    #:json
    (hasheq 'model "gpt-3.5-turbo"
            'messages (list (hash 'role "user" 'content prompt 'stream #f)))
    #:timeouts (make-timeout-config #:request 120)))
  #;(displayln (response-json rsp))

  (define response-hash (response-json rsp))
  (define usage (hash-ref response-hash 'usage))

  (log-model-cost!
   (cost-log-entry
    gpt3-cost-info
    (inference-cost-info
     (hash-ref usage 'prompt_tokens)
     (hash-ref usage 'completion_tokens)
     #f
     #f)))

  (hash-ref (hash-ref (list-ref (hash-ref response-hash 'choices) 0) 'message) 'content))

(current-send-prompt! gpt3-5-send-prompt!)
