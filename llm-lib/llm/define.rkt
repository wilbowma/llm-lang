#lang racket/base

(require
 (for-syntax
  racket/base
  syntax/parse
  racket/function
  racket/port
  with-cache
  racket/file
  llm))

(provide (for-syntax (all-from-out llm)) define-by-prompt!)

(define-syntax (define-by-prompt! syn)
 (syntax-parse syn
  ; TODO: no reason in principle this should be a string literal.
  [(_ name:id (~optional (~seq #:output-file file:str)) prompt:str ...)
   (define prompt-str (string-append (apply string-append (map syntax->datum (attribute prompt))) "\nReturn only unformatted code without explanation. Do not use Markdown. Do not include the `#lang` line."))
   (define name-str (format "~a" (syntax->datum #'name)))
   #`(begin
      #,@(map
          (curry datum->syntax syn)
          (with-input-from-string
	   (if (attribute file)
               (let ([file (syntax->datum #'file)])
                 (if (file-exists? file)
                     (file->string file)
                     (let ([resp (prompt! prompt-str)])
                       (with-output-to-file file
                        (lambda () (display resp))
                        #:mode 'text #:exists 'replace)
                        resp)))
               (prompt! prompt-str))
           (lambda ()
            (let loop ([defs '()])
             (let ([d (read)])
              (if (eof-object? d) (reverse defs) (loop (cons d defs))))))))
      #;(unless name (error (format "Prompt did not define ~a correctly" 'name))))]))
