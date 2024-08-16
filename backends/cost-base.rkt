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
  (current-cost-port))
 log)

;; a logger is a Log -> Log function, so loggers can be composed.
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
    (MWh . ,(cons (lambda (v) ((between/c 1 5) (order-of-magnitude v))) (curryr / 1000)))
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
  (check-equal? unit 'MWh)
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

; GPT written
; ---

(define (round-to-n x n)
  (if (zero? x)
      0
      (let* ([magnitude (floor (log (abs x) 10))]
             [scale (expt 10 (- magnitude (- n 1)))]
             [scaled-x (/ x scale)]
             [rounded-x (round scaled-x)])
        (* rounded-x scale))))

; end of GPT written
; ---

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

  (define render-nums (compose (curryr ~r #:group-sep ",") (curryr round-to-n 2)))

  (with-output-to-string
   (thunk

    ;; Session Table
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

    ;; Training Table
    (define-values (training-power-unit training-power-cost) (kWh->xWh-search kwh-training))
    (define-values (training-co2-unit training-co2-cost) (tCO2->xCO2-search co2-training))
    (define-values (training-water-unit training-water-cost) (Lwater->xwater-search L-training))
    (displayln "One-time Training Costs")
    (draw-table-as-raart-table-here
     `((,(format "Power (~a)" training-power-unit)
        ,(format "Carbon (~a)" training-co2-unit)
        ,(format "Water (~a)" training-water-unit))
       ,(map render-nums `(,training-power-cost ,training-co2-cost ,training-water-cost))))

    ;; Context Table
    (displayln "References Resource Usage, for Context")
    (draw-table-as-raart-table-here
     `((Reference Power Carbon Water)
       ; https://www.eia.gov/energyexplained/use-of-energy/electricity-use-in-homes.php
       ,(let-values ([(power-unit power-value) (kWh->xWh-search 10500)]
                     ; https://www.epa.gov/watersense/how-we-use-water
                     [(water-unit water-value) (Lwater->xwater-search 1135.62)]
                     ; https://css.umich.edu/publications/factsheets/sustainability-indicators/carbon-footprint-factsheet
                     [(carbon-unit carbon-value) (tCO2->xCO2-search 48)])
          `("1 US Household (annual)"
            ,(format "~a~a" (render-nums power-value) power-unit)
            ,(format "~a~a" (render-nums carbon-value) carbon-unit)
            ,(format "~a~a" (render-nums water-value) water-unit)))
        ; https://theicct.org/sites/default/files/publications/CO2-commercial-aviation-oct2020.pdf
        ; all metric units with conversion factors.
        ; also table 9 figures on emissions per flight of various aircraft
        ; https://www.aircraft-commerce.com/wp-content/uploads/aircraft-commerce-docs/Flight%20Operations/2018/117_FLTOPS_A.pdf
        ; Using Airbus A321neo, which is
        ; NY to JFK is 2999 nm, 5555km
        ; A321neo use 0.0107 fuel per ASM for ~2knm, with 192 seats
        ; so .0107*192*2999 should yield fuel = 6,161.1456 in.. USG?
        ; by conversion, 1 USG fuel -> 3.785 L fuel
        ; 1 L fuel -> .8 kg fuel
        ; 1 kg fuel -> 3.16 kgCO2
        ; 1 kgCO2 -> .001 tCO2
        ,(let-values ([(carbon-unit carbon-value) (tCO2->xCO2-search (* 6161.1456 3.785 .8 3.16 .001))])
           `("1 JFK -> LHR Flight"
             ""
             ,(format "~a~a" (render-nums carbon-value) carbon-unit)
             ""))

        ; https://www.eia.gov/electricity/annual/html/epa_04_03.html
        ; 6542 generators at total 565950 MW capacity = 86.5102415164MW
        ; capcity factors from here: https://www.eia.gov/electricity/annual/html/epa_04_08_a.html
        ; 2022 average here 56% : https://www.eia.gov/todayinenergy/detail.php?id=61444
        ; 2022 average heat rate: 7740; close to 45%:
        ; https://www.eia.gov/tools/faqs/faq.php?id=107&t=3
        ; https://www.eia.gov/electricity/annual/html/epa_08_01.html
        ; 86.5102415164*.56*.45*hours in a year =
        ,(let*-values ([(avg-kWh) (* 86.51 .56 .45 8766 1000)]
                       ; https://m.dw.com/en/how-sustainable-is-wind-power/a-60268971 442 grams CO2 per kWh for natural gas
                       [(avg-tCO2) (/ (* 422 avg-kWh) (* 1000 1000))]
                       ;  2,793 gal/MWh in 2020 https://www.eia.gov/todayinenergy/detail.php?id=50698
                       [(avg-water) (* 2793 (/ avg-kWh 1000) 3.79)]
                       [(power-unit power-value) (kWh->xWh-search avg-kWh)]
                       [(carbon-unit carbon-value) (tCO2->xCO2-search avg-tCO2)]
                       [(water-unit water-value) (Lwater->xwater-search avg-water)])
         `("1 Avg. Natural Gas Plant (annual)"
           ,(format "-~a~a" (render-nums power-value) power-unit)
           ,(format "~a~a" (render-nums carbon-value) carbon-unit)
           ,(format "~a~a" (render-nums water-value) water-unit)))

        ; per capital US usage:
	; https://ourworldindata.org/per-capita-co2
        ; https://ourworldindata.org/water-use-stress
        ; https://ourworldindata.org/grapher/per-capita-energy-use
        ,(let-values ([(power-unit power-value) (kWh->xWh-search 77028)]
                      [(carbon-unit carbon-value) (tCO2->xCO2-search 14.9)]
                      [(water-unit water-value) (Lwater->xwater-search (* 1543 1000))])
           `("US per capita (annual)"
             ,(format "~a~a" (render-nums power-value) power-unit)
             ,(format "~a~a" (render-nums carbon-value) carbon-unit)
             ,(format "~a~a" (render-nums water-value) water-unit))))))))
