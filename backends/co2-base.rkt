#lang racket/base

(provide
 fprintf-co2-info
 make-co2-info
 displayln-co2-info)

(struct model-energy-params (alpha-k-s-0 alpha-k-s-1 alpha-k-s-2))

(struct co2-info (model-params  input-tokens output-tokens training-co2))
