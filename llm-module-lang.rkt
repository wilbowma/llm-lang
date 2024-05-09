#lang racket/base

(require 
 "backends/config.rkt"
 racket/port
 racket/match
 (for-syntax racket/base syntax/parse))

(provide 
 prompt!
 current-response-timeout
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
  [_ (void (fprintf (current-prompt-port) "~a" e))]))

(define (prompt!)
 (define prompt (bytes->string/utf-8 (get-output-bytes (current-prompt-port) #t) #\uFFFD))
 (unless (equal? "" prompt)
   ((current-send-prompt!) prompt)))

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
         (wrap e) ...
         (prompt!))]))

(define-syntax (new-top-interaction stx)
  (syntax-parse stx
    [(_ . e)
     #`(begin (wrap-f e) (prompt!))]))
