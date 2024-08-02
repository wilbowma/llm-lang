#lang racket/base

(require
 racket/match
 racket/set)

(provide
 (struct-out model-cost-info)
 (struct-out model-energy-params)
 model-cost-info->string
 current-cost-port)

(struct model-energy-params (alpha-k-s-0 alpha-k-s-1 alpha-k-s-2))

(struct model-cost-info (model-name query-tco2/kwh model-params input-tokens output-tokens training-tco2 training-kwh))

(define AVERAGE-ANNUAL-KWH-AMERICAN-HOUSE 10500)

(define (model-cost-info->co2 info)
 (define-values (kwh-query _) (model-cost-info->kwh info))
 (values (* kwh-query (model-cost-info-query-tco2/kwh info)) (model-cost-info-training-tco2 info)))

(define JOULES/KWH 2.77778e-7)

(define (model-cost-info->kwh info)
 (match-define
  (model-cost-info _3 _1 (model-energy-params alpha-k-s-0 alpha-k-s-1 alpha-k-s-2) input-tokens output-tokens _2 training-kwh)
  info)
 (define kwh-query
  (* JOULES/KWH (+ (* alpha-k-s-0 input-tokens) (* alpha-k-s-1 output-tokens) (* alpha-k-s-2 input-tokens output-tokens))))
 (values kwh-query training-kwh))

(define (model-cost-info->string info)
  (define-values (co2-query co2-training) (model-cost-info->co2 info))
  (define-values (kwh-query kwh-training) (model-cost-info->kwh info))
  (string-append
   (format "This query cost ~a tCO2, with training cost of ~a tCO2~n" co2-query co2-training)
   #;(let-values ([(co2-emitter co2-emission) (co2-order-of-magntitude co2-query)])
   (format "For reference, they query is close to the CO2 emitted by ~a (~a tCO2)~n"
    co2-emitter co2-emission))
   (format "This query cost ~a KWh, with training cost of ~a KWh~n"
    kwh-query kwh-training)
   (format "For reference, this query could power an average American house for ~a years (and ~a years for the training costs)~n"
    (/ kwh-query AVERAGE-ANNUAL-KWH-AMERICAN-HOUSE)
    (/ kwh-training AVERAGE-ANNUAL-KWH-AMERICAN-HOUSE))))

(provide
 string-stderr-model-cost-logger
 log-model-cost!
 log->string
 current-model-cost-logger
 current-model-cost-log)

(define current-model-cost-log (make-parameter '()))
(define current-model-cost-logger (make-parameter (void)))

(define current-cost-port (make-parameter (current-error-port)))

(define (log-model-cost! info)
 (let ([new-log (cons info (current-model-cost-log))])
  (current-model-cost-log new-log)
  ((current-model-cost-logger) new-log)))

(define (log->string log)
 (define-values (co2-query kwh-query co2-training-set kwh-training-set)
  (for/fold ([co2-query-cum 0]
             [kwh-query-cum 0]
             ; probably should be hashes, mapping model names to costs
             [co2-training-set (set)]
             [kwh-training-set (set)])
            ([info log])
   (let-values ([(co2-query co2-training) (model-cost-info->co2 info)]
                [(kwh-query kwh-training) (model-cost-info->kwh info)])
    (values (+ co2-query-cum co2-query) (+ kwh-query-cum kwh-query)
     (set-add co2-training-set co2-training)
     (set-add kwh-training-set kwh-training)))))
  (define kwh-training (for/sum ([i kwh-training-set]) i))
  (define co2-training (for/sum ([i co2-training-set]) i))
  (string-append
   (format "Cumulative query costs for this session are ~a tCO2, relying on total one-time training cost of ~a tCO2~n" co2-query co2-training)
   #;(let-values ([(co2-emitter co2-emission) (co2-order-of-magntitude co2-query)])
   (format "For reference, they query is close to the CO2 emitted by ~a (~a tCO2)~n"
    co2-emitter co2-emission))
   (format "Cumulative query costs for this session are ~a KWh, relying on total one-time training costs of ~a KWh~n"
    kwh-query kwh-training)
   (format "For reference, this session could power an average American house for ~a years (and ~a years for the training costs)~n"
    (/ kwh-query AVERAGE-ANNUAL-KWH-AMERICAN-HOUSE)
    (/ kwh-training AVERAGE-ANNUAL-KWH-AMERICAN-HOUSE))))

(define (string-stderr-model-cost-logger log)
  (displayln
   (log->string (current-model-cost-log))
   (current-cost-port)))
