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

;; TODO
;; Would be nice to use display for the result of a prompt, but print for other values.
;; Could introduce a "display-string" and special case it?
;; Using this for now, for backwards compatibility.
(current-print (lambda (e) (if (void? e) (void) (displayln e))))

(define (pquote e)
 (match e
  ; some conveniences, for implicit unprompting irrelevant values.
  ["" (void)]
  ;["\n" (void)]
  [(? void?) (void)]
  [_ (append-prompt! (format "~a" e))]))

(define-syntax (punquote stx)
 (syntax-parse stx
  [(_ e)
   ; hack to cooperate with the hack for require
   (if (eq? (syntax-local-context) 'top-level)
       #'e
       (raise-syntax-error 'unprompt "Cannot use punquote outside pquasiquote"))]))

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
     (with-handlers ([values (lambda (exn) (raise exn))])
      (with-handlers ([values (lambda (exn) (local-expand #'e 'top-level '()))])
       (let-values ([(_ x) (syntax-local-expand-expression #'(pquasiquote e))])
         x)))]))

(require (prefix-in scrb: scribble/reader))

(define scribble-like-read-interaction
 (lambda (fo e)
  (syntax-case (scrb:read-syntax-inside fo e) ()
   [() eof]
   [(str ... newline) #'(str ...)])))

(define-syntax (new-module-begin stx)
  (syntax-parse stx
    [(_ e ...)
     #`(#%module-begin
	(current-read-interaction scribble-like-read-interaction)
        (top-pquasiquote e) ...
        (prompt!))]))

(define-syntax (new-top-interaction stx)
  (syntax-parse stx
    [(_ e ...)
     #`(values (pquasiquote e) ... (prompt!))]))
