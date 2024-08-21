#lang racket/base

(require 
 net/http-easy
 "config.rkt"
 "cost-base.rkt"
 openssl/md5)

(provide base-send-prompt!)

(define (base-send-prompt! uri headers json cost-base-info inference-cost-maker)
  (define timeout (current-response-timeout))

  (log-llm-lang-debug "Request headers for prompt: ~a" headers)
  (log-llm-lang-debug "Timeout set to ~a" timeout)
  (log-llm-lang-debug "Posting ~a to ~a" json uri)

  (define rsp
   (post uri #:headers headers #:json json #:timeouts (make-timeout-config #:request timeout)))

  ;; TODO add error handling

  (define response-hash (response-json rsp))
  (log-llm-lang-debug "Response JSON: ~a" response-hash)
  (log-model-cost!
   (cost-log-entry
    cost-base-info
    (inference-cost-maker response-hash)))

  response-hash)

#;(define (cached-send-prompt! uri headers json cost-base-info inference-cost-maker prompt)
 (with-cache (cachefile (md5 (open-input-string prompt)))
  (lambda ()
   (base-send-prompt uri headers json cost-base-info inference-cost-maker))
  #:keys (list current-llm-backend-name (lambda () uri) (lambda () headers) (lambda () json) current-response-timeout prompt)))
