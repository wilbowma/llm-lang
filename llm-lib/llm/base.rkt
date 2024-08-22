#lang racket/base

(require
 net/http-easy
 "config.rkt"
 "cost-base.rkt"
 openssl/md5
 with-cache)

(provide
 base-send-prompt!
 cached-send-prompt!)

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

(define (cached-send-prompt! uri headers json cost-base-info inference-cost-maker prompt messages)
 (define name (current-llm-backend-name))
 (define prompt-hash (md5 (open-input-string (format "~a-~a" prompt messages))))
 (define log-file-name (format "~a-~a-replay.log" name prompt-hash))
 (with-cache (cachefile log-file-name)
  (lambda ()
   (base-send-prompt! uri headers json cost-base-info inference-cost-maker))
  #:keys (list current-llm-backend-name (lambda () uri) (lambda () json) current-response-timeout (lambda () prompt) (lambda () messages)))
  #|
  ;; Could hook into read and write to log the current cost, but I think for now the behaviour of *not* reporting cost on a cached response is good enough.
  #:write (lambda (e)
	   (with-output-to-file (format "~a-~a-cost.log" name prompt-hash)
	    ;; sexp->fasl?
	    (lambda () (write (serialize (current-model-cost-log)))))
	   (serialize e))
  |#
  )
