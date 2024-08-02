#lang llm-lang

@(require llm-lang/backends/ollama/phi3)

@(current-response-timeout 300)

Please write a quicksort function definition, named "quicksort", in Racket.

Do not explain or send any other text; only return the code.

Do not return Markdown; return only plaintext.
@; The LLM tends to ignore this instruction.

@(require markdown/parse racket/match)

@(define (extract-code p)
   (match p
     [`(p ,_ ,body)
      (extract-code body)]
     [`(pre ,_ ,body)
      (extract-code body)]
     [`(code ,_ ,body)
      (extract-code body)]
     [(? string?)
      p]
     [_ (error "Couldn't parse result" p)]))

@(define (parse-code-prompt!)
   (read (open-input-string (extract-code (car (parse-markdown (prompt!)))))))

@; the LLM sometimes produces invalid syntax and errors here.
@(define quicksort-code (parse-code-prompt!))

@(require racket/pretty)
@(pretty-display quicksort-code)

@(define quicksort
  (eval `(begin ,quicksort-code quicksort)
        (module->namespace 'racket)))

@(displayln (quicksort '(1 0 8 6 3 1 0 2)))