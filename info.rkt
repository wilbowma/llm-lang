#lang info
(define collection "llm-lang")
(define deps '("base" "scribble-lib" "http-easy-lib" "raart" "markdown" "with-cache"))
(define build-deps '("racket-doc" "rackunit-lib"))
;(define scribblings '(("scribblings/llm-lang.scrbl" ())))
(define pkg-desc "A prototype LLM-first programming language.")
(define version "0.1")
(define pkg-authors '(wilbowma))
(define license '(MIT))
