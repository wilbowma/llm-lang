#lang racket/base

(require
 "../config.rkt"
 "../cost-base.rkt"
 racket/port
 racket/match
 (for-syntax racket/base syntax/parse))

(provide
 prompt!
 append-prompt!
 current-llm-backend-name
 current-response-timeout
 current-model-cost-logger
 llm-lang-logger
 current-power-use
 current-carbon-use
 current-water-use
 (rename-out
  [new-module-begin #%module-begin]
  [new-top-interaction #%top-interaction])
 (except-out
  (all-from-out racket/base)
  #%module-begin
  #%top-interaction))

(define (wrap-f e)
 (match e
  [(? void?) e]
  ["\n" (void)]
  [_ (append-prompt! e)]))

(define-syntax (wrap stx)
  (syntax-parse stx
    [(_ e)
     (with-handlers ([values (lambda _ #'e)])
       (let-values ([(_ x) (syntax-local-expand-expression #'e)])
         #`(wrap-f #,x)))]))

(define-syntax (new-module-begin stx)
  (syntax-parse stx
    [(_ e ...)
     #`(#%module-begin
        ;; TODO: why is this not just wrap-f?
        ;; doesn't work with require!
        (wrap e) ...
        (display (prompt!)))]))

(define-syntax (new-top-interaction stx)
  (syntax-parse stx
    [(_ . e)
     #`(begin (wrap-f e) (display (prompt!)))]))

(require scribble/reader)
(define (wrap2 str)
 (match str
  [(? void?) ""]
  [_ (format "~a" str)]))

(current-read-interaction
  (lambda (fo e)
    (syntax-case (read-syntax-inside fo e) ()
      [() eof]
      [(str ...)
       #'(string-append (wrap2 str) ...)])))
