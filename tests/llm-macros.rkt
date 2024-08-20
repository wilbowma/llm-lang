#lang racket/base

(require 
 (for-syntax with-cache)
 (for-syntax llm-lang/llm-module-lang) 
 (for-syntax syntax/parse racket/port racket/function))

(provide (for-syntax (all-from-out llm-lang/llm-module-lang)) define-by-prompt)
(define-syntax (define-by-prompt syn)
 (syntax-parse syn
  ; no reason in principle this should be a string literal.
  [(_ name:id prompt:str)
   (define prompt-str (string-append (syntax->datum #'prompt) "\nReturn only unformatted code without explanation. Do not use Markdown. Do not include the `#lang` line."))
   (define name-str (format "~a" (syntax->datum #'name)))
   #`(begin
       #,@(map (curry datum->syntax syn) (with-input-from-string 
              (with-cache (format "~a-code.rktd" name-str) 
               (lambda () (prompt! prompt-str))
               #:keys (list (lambda () prompt-str) (lambda () name-str) current-llm-backend-name))
              (lambda ()
 		(let loop ([defs '()])
                 (let ([d (read)])
                  (if (eof-object? d) (reverse defs) (loop (cons d defs))))))))
       (unless name (error "Prompt did not define ~a correctly" #,name-str)))]))
