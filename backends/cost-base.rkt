#lang racket/base

(require
 racket/match
 racket/set
 racket/generic)

(provide
 (struct-out model-cost-info)
 (struct-out inference-cost-info)
 (struct-out wilkins-inference-model)
 (struct-out time-avg-inference-model)
 (struct-out cost-log-entry)
 current-cost-port
 current-system-kw)

(struct inference-cost-info (input-tokens output-tokens prompt-duration response-duration))

(define-generics kwh-model
 (model->kwh kwh-model info))

(define JOULES/KWH 2.77778e-7)

(struct wilkins-inference-model (alpha-k-s-0 alpha-k-s-1 alpha-k-s-2)
 #:methods gen:kwh-model
 [(define (model->kwh model info)
   (match* (model info)
    [((wilkins-inference-model alpha-k-s-0 alpha-k-s-1 alpha-k-s-2)
      (inference-cost-info input-tokens output-tokens _1 _2))
     (* JOULES/KWH (+ (* alpha-k-s-0 input-tokens) (* alpha-k-s-1 output-tokens) (* alpha-k-s-2 input-tokens output-tokens)))]))])

(define NANOSECONDS/HOURS 3.6e+12)

;; A wild guess
(define current-system-kw (make-parameter .5))

(struct time-avg-inference-model (kw)
 #:methods gen:kwh-model
 [(define (model->kwh model info)
  (match* (model info)
   [((time-avg-inference-model kw)
     (inference-cost-info _1 _2 prompt-duration response-duration))
    (exact->inexact (* kw (/ (+ prompt-duration response-duration) NANOSECONDS/HOURS)))]))])

(struct model-cost-info (model-name query-tco2/kwh query-water-L/kWh training-water-L/kWh training-tco2 training-kwh inference-model))

(provide
 string-stderr-model-cost-logger
 log-model-cost!
 log->string
 current-model-cost-logger
 current-model-cost-log)

;; TODO: Could reduce memory footprint of log by logging model-cost-info once, rather than per entry.
;; Or using pair
(struct cost-log-entry (model-cost-info inference-info))

(define (cost-entry->co2 entry)
 (match-define
  (cost-log-entry (model-cost-info _1 query-tco2/kwh _3 _4 training-tco2 _2 inference-model) inference-info)
  entry)
 (values (* (model->kwh inference-model inference-info) query-tco2/kwh) training-tco2))

(define (cost-entry->kwh entry)
 (match-define
  (cost-log-entry (model-cost-info _3 _1 _2 _4 _5 training-kwh inference-model) inference-info)
  entry)
 (values (model->kwh inference-model inference-info) training-kwh))

(define (cost-entry->L entry)
 (define-values (query training) (cost-entry->kwh entry))
 (define query-L/kWh (model-cost-info-query-water-L/kWh (cost-log-entry-model-cost-info entry)))
 (define training-L/kWh (model-cost-info-training-water-L/kWh (cost-log-entry-model-cost-info entry)))
 (values (* query-L/kWh query) (* training-L/kWh training)))

(define current-model-cost-log (make-parameter '()))

(define (string-stderr-model-cost-logger log)
  (displayln
   (log->string (current-model-cost-log))
   (current-cost-port)))

(define current-model-cost-logger (make-parameter string-stderr-model-cost-logger))

(define current-cost-port (make-parameter (current-error-port)))

(define (log-model-cost! entry)
 (let ([new-log (cons entry (current-model-cost-log))])
  (current-model-cost-log new-log)
  ((current-model-cost-logger) new-log)))

(define AVERAGE-ANNUAL-KWH-AMERICAN-HOUSE 10500)

(define (log->string log)
 (define-values (co2-query kwh-query L-query co2-training-set kwh-training-set L-training-set)
  (for/fold ([co2-query-cum 0]
             [kwh-query-cum 0]
             [L-query-cum 0]
             ; probably should be hashes, mapping model names to costs
             [co2-training-set (set)]
             [kwh-training-set (set)]
             [L-training-set (set)])
            ([entry log])
   (let-values ([(co2-query co2-training) (cost-entry->co2 entry)]
                [(kwh-query kwh-training) (cost-entry->kwh entry)]
                [(L-query L-training) (cost-entry->L entry)])
    (values 
     (+ co2-query-cum co2-query) 
     (+ kwh-query-cum kwh-query) 
     (+ L-query-cum L-query)
     (set-add co2-training-set co2-training)
     (set-add kwh-training-set kwh-training)
     (set-add L-training-set L-training)))))
  (define kwh-training (for/sum ([i kwh-training-set]) i))
  (define co2-training (for/sum ([i co2-training-set]) i))
  (define L-training (for/sum ([i L-training-set]) i))
  (string-append
   (format "Cumulative query costs for this session are ~a tCO2, relying on total one-time training cost of ~a tCO2~n" co2-query co2-training)
   #;(let-values ([(co2-emitter co2-emission) (co2-order-of-magntitude co2-query)])
   (format "For reference, they query is close to the CO2 emitted by ~a (~a tCO2)~n"
    co2-emitter co2-emission))
   (format "Cumulative query costs for this session are ~a KWh, relying on total one-time training costs of ~a KWh~n"
    kwh-query kwh-training)
   (format "For reference, this session could power an average American house for ~a years (and ~a years for the training costs)~n"
    (/ kwh-query AVERAGE-ANNUAL-KWH-AMERICAN-HOUSE)
    (/ kwh-training AVERAGE-ANNUAL-KWH-AMERICAN-HOUSE))
   (format "Cumulative direct on-site cooling water costs for this session are ~a L, relying on total one-time training costs of ~a L~n"
    L-query L-training)))