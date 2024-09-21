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
 punquote
 (rename-out [punquote unprompt])
 (rename-out
  [new-module-begin #%module-begin]
  [new-top-interaction #%top-interaction])
 (except-out
  (all-from-out racket/base)
  #%module-begin
  #%top-interaction))

(current-print (lambda (e) (if (void? e) (void) (displayln e))))

(define (pquote e)
 (match e
  ; some conveniences, for implicit unprompting irrelevant values.
  ;["" (void)]
  ;["\n" (void)]
  ;[(? void?) (void)]
  [_ (append-prompt! (format "~a" e))]))

(define-syntax (punquote stx)
 (raise-syntax-error "Cannot use punquote outside pquasiquote"))

(define-syntax (pquasiquote stx)
 (syntax-parse stx
  [(_ ((~literal punquote) e))
   #`e]
  [(_ e)
   #`(pquote e)]
  [(_ e e^ ...)
   #`(begin (pquasiquote e) (pquasiquote e^) ...)]))

(define-syntax (top-pquasiquote stx)
  (syntax-parse stx
    [(_ e)
     ; hack to handle top-level stuff like `require`:
     ; try to expand as expression, and avoid quasiquoting if it's not valid in expression context
     (with-handlers ([values (lambda _ #'e)])
       (let-values ([(_ x) (syntax-local-expand-expression #'(pquasiquote e))])
         x))]))

(define-syntax (new-module-begin stx)
  (syntax-parse stx
    [(_ e ...)
     #`(#%module-begin
        ;; TODO: why is this not just wrap-f?
        ;; doesn't work with require!
        (top-pquasiquote e) ...
        (prompt!))]))

(require racket/trace)
(trace-define-syntax (new-top-interaction stx)
  (syntax-parse stx
    [(_ e ...)
     #`(begin ((current-print) (pquasiquote e)) ... (prompt!))]))

(require scribble/reader)

(current-read-interaction
  (lambda (fo e)
    (syntax-case (read-syntax-inside fo e) ()
      [() eof]
      [(str ... newline)
       #'(str ...)])))
