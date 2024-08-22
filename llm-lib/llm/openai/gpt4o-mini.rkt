#lang racket/base

(require
 net/http-easy
 racket/port
 "config.rkt"
 "../base.rkt"
 "../config.rkt"
 "../cost-base.rkt")

(provide OPENAI_API_KEY)

(current-llm-backend-name 'gpt4o-mini)

; Estimates of GPT-4 say 1.8 trillion parameters. Probably GPT4o-mini is an optimization of GPT 4, and its training cost is lower bounded by GPT 4.
; LLMCarbon's with a bunch of leaked unverfied data about GPT 4.
; https://github.com/llv22/gpt4_essay/blob/master/GPT-4-9.JPG
(define gpt4-training-tco2
 3530.575367647057)

(define gpt4-training-kwh
 (* 1000 8229.779411764703))

; Continuing to use GPT 3-turbo. This is probably an overestimate, because the inference cost has been optimized for 4o-mini.
;
; Using Llama-2 13b as estimate. The parameter count of GPT3.5 Turbo, isn't public, but it's guessed to be 20B.
; But, it's also apparently heavily optimized. Better to underestimate, I guess?
;
; https://x.com/labenz/status/1654853390839472134/photo/1
; https://www.reddit.com/r/LocalLLaMA/comments/17lvquz/comment/k7hjc8b/
;
; This seems to line up with estimates of energy usage of GPT 3 from brown2020a, which estimated .004kwh usage per request.
; I'm getting .002 for small examples, so if we assume some constant overhead and/or optimization, or the input tokens matter, this seems to be validated.
(define gpt4-inference-model (wilkins-inference-model -6.79 56.01 (* 7.29 (exp -2))))

; https://ourworldindata.org/grapher/carbon-intensity-electricity
(define tco2/kwh (* 170 1.102311e-6))

;; using US average of Microsoft's servers from li2023
(define L/kWh .55)

(define gpt4-cost-info
 (model-cost-info 'gpt4o-mini tco2/kwh L/kWh L/kWh gpt4-training-tco2 gpt4-training-kwh gpt4-inference-model))

(provide gpt4-add-image!)

(define current-gpt4-images (make-parameter '()))

(define (gpt4-add-image! type base64-data)
 (current-gpt4-images (cons (hasheq 'type "image_url" 'image_url (hasheq 'url (format "data:image/~a;base64,~a" type base64-data))) (current-gpt4-images))))

(define (gpt4o-mini-send-prompt! prompt)
  (define response-hash
   (cached-send-prompt!
    "https://api.openai.com/v1/chat/completions"
    (hasheq 'Authorization (format "Bearer ~a" (OPENAI_API_KEY)))
    (hasheq 'model "gpt-4o-mini"
     'messages (list (hasheq 'role "user"  'stream #f
		     'content (cons (hasheq 'type "text" 'text prompt)
			     (current-gpt4-images))))
     gpt4-cost-info
     (lambda (rsp-hash)
      (define usage (hash-ref rsp-hash 'usage))
      (inference-cost-info
       (hash-ref usage 'prompt_tokens)
       (hash-ref usage 'completion_tokens)
       #f
       #f)))
    prompt))

  (current-gpt4-images '())

  (hash-ref (hash-ref (list-ref (hash-ref response-hash 'choices) 0) 'message) 'content))

(current-send-prompt! gpt4o-mini-send-prompt!)
