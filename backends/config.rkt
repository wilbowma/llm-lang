#lang racket/base

(provide (all-defined-out))

(define current-prompt-port (make-parameter (open-output-string)))

(define current-send-prompt! (make-parameter #f))


