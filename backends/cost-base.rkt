#lang racket/base

(require
 racket/match
 racket/set
 racket/generic
 raart
 racket/function
 racket/port
 racket/format
 racket/math
 racket/dict
 racket/contract)

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

(define (draw-table-as-raart-table-here t)
 (draw-here
  (table
   (for/list ([row t])
    (map (compose text (curry format "~a")) row)))))

(define (unit-search unit conversion-hash v)
 ; zero breaks order-of-magnitude
 (if (zero? v)
     (values unit v)
     (for/fold ([unit unit] [v v])
              ([(new-unit info) (in-dict conversion-hash)]
               #:do [(match-define (cons pred conv) info)]
               #:final (pred v)
               #:unless (not (pred v)))
      (values new-unit (conv v)))))

(define kWh->xWh-search
 (curry unit-search 'kWh
  ; preds must uniquely determine
  `((GWh . ,(cons (lambda (v) (>= (order-of-magnitude v) 6)) (curryr / (* 1000 1000))))
    (mWh . ,(cons (lambda (v) ((between/c 1 5) (order-of-magnitude v))) (curryr / 1000)))
    (Wh . ,(cons (lambda (v) (<= (order-of-magnitude v) 0)) (curry * 1000))))))

(define tCO2->xCO2-search
 (curry unit-search 'tCO2
  `((mgCO2 . ,(cons (lambda (v) (<= (order-of-magnitude v) -9)) (curryr * 1000 1000 1000)))
    (gCO2 . ,(cons (lambda (v) ((between/c -8 -4) (order-of-magnitude v))) (curryr * 1000 1000)))
    (kgCO2 . ,(cons (lambda (v) ((between/c -3 -2) (order-of-magnitude v))) (curryr * 1000))))))

(define Lwater->xwater-search
 (curry unit-search 'L
  `((mL . ,(cons (lambda (v) (<= (order-of-magnitude v) -3)) (curryr * 1000))))))

(module+ test
 (require rackunit)
 (let-values ([(unit v) (kWh->xWh-search 6000)])
  (check-equal? unit 'mWh)
  (check-equal? v 6))

 (let-values ([(unit v) (kWh->xWh-search .001)])
  (check-equal? unit 'Wh)
  (check-equal? v 1.0))

 (let-values ([(unit v) (tCO2->xCO2-search 1000)])
  (check-equal? unit 'tCO2)
  (check-equal? v 1000))

 (let-values ([(unit v) (tCO2->xCO2-search 1)])
  (check-equal? unit 'tCO2)
  (check-equal? v 1))

 (let-values ([(unit v) (tCO2->xCO2-search .1)])
  (check-equal? unit 'tCO2)
  (check-equal? v .1))

 (let-values ([(unit v) (tCO2->xCO2-search .001)])
  (check-equal? unit 'kgCO2)
  (check-equal? v 1.0))

 (let-values ([(unit v) (tCO2->xCO2-search .00001)])
  (check-equal? unit 'gCO2)
  (check-equal? v 10.0))
)

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

  (define render-nums (curryr ~r #:precision 2 #:group-sep ","))

  (with-output-to-string
   (thunk

    (define-values (query-power-unit query-power-cost) (kWh->xWh-search kwh-query))
    (define-values (query-co2-unit query-co2-cost) (tCO2->xCO2-search co2-query))
    (define-values (query-water-unit query-water-cost) (Lwater->xwater-search L-query))
    (displayln "Cumulative Query Session Costs")
    (draw-table-as-raart-table-here
     `((,(format "Power (~a)" query-power-unit)
        ,(format "Carbon (~a)" query-co2-unit)
        ,(format "Water (~a)" query-water-unit))
       ,(map render-nums `(,query-power-cost ,query-co2-cost ,query-water-cost))))
    (newline)

    (define-values (training-power-unit training-power-cost) (kWh->xWh-search kwh-training))
    (define-values (training-co2-unit training-co2-cost) (tCO2->xCO2-search co2-training))
    (define-values (training-water-unit training-water-cost) (Lwater->xwater-search L-training))
    (displayln "One-time Training Costs")
    (draw-table-as-raart-table-here
     `((,(format "Power (~a)" training-power-unit)
        ,(format "Carbon (~a)" training-co2-unit)
        ,(format "Water (~a)" training-water-unit))
       ,(map render-nums `(,training-power-cost ,training-co2-cost ,training-water-cost)))))))
