#lang racket/base

(require racket/list racket/string)

(provide table->string draw-table)

(module+ test
 (require rackunit))

(define (get-column-widths rows)
 (for/list ([col (range (length (car rows)))])
  (apply max (map (lambda (row) (string-length (list-ref row col))) rows))))

(module+ test
 (check-equal?
  (get-column-widths '(("A" "BB" "CCC") ("a" "b" "c")))
  '(1 2 3))

(check-equal?
  (get-column-widths '(("A" "B" "C") ("aaa" "b" "ccccc")))
  '(3 1 5)))

(define (draw-row row widths left-corner col-divider right-corner)
 (string-append
  (string-join
   (for/list ([cell row]
              [width widths]
              [i (in-naturals)])
    (define l (if (zero? i) left-corner col-divider))
    (string-append l cell (make-string (- width (string-length cell)) #\space)))
   "")
  right-corner))

(define top-right-corner "┐")
(define top-left-corner "┌")
(define bottom-right-corner "┘")
(define bottom-left-corner "└")
(define left-wall "│")
(define left-row-wall "├")
(define right-wall "│")
(define right-row-wall "┤")
(define col-divider "│")
(define row-divider "─")
(define top-col-divider "┬")
(define bottom-col-divider "┴")
(define row-col-divider "┼")

(module+ test
 (check-equal?
  (draw-row '("A" "BB" "CCC") '(1 2 3) "|" "|" "|")
  "|A|BB|CCC|")

 (check-equal?
  (draw-row '("A" "BB" "CCC") '(3 3 3) "|" "|" "|")
  "|A  |BB |CCC|")

 (check-equal?
  (draw-row '("A" "BB" "CCC") '(3 3 3) top-left-corner col-divider top-right-corner)
  "┌A  │BB │CCC┐"))

(define (draw-divider widths left row col-divider right)
  (string-append
   (string-join
    (for/list ([w widths]
               [i (in-naturals)])
     (define l (if (zero? i) left col-divider))
     (string-append l (apply string-append (build-list w (lambda _ row)))))
    "")
   right))

(module+ test
 (check-equal?
  (draw-divider '(1 2 3) "+" "-" "|" "+")
  "+-|--|---+")

 (check-equal?
  (draw-divider '(1 1 1) top-left-corner row-divider top-col-divider top-right-corner)
  "┌─┬─┬─┐")

 (check-equal?
  (draw-divider '(1 1 1) bottom-left-corner row-divider bottom-col-divider bottom-right-corner)
  "└─┴─┴─┘"))

(define (draw-table rows)
 (display (table->string rows)))

(define (table->string rows)
  (define widths (get-column-widths rows))
  (string-join
   `(,@(for/list ([row rows]
                  [i (in-naturals)])
        (string-append
	 (if (zero? i)
	     (draw-divider widths top-left-corner row-divider top-col-divider top-right-corner)
             (draw-divider widths left-row-wall row-divider row-col-divider right-row-wall))
         "\n"
	 (draw-row row widths left-wall col-divider right-wall)))
     ,(draw-divider widths bottom-left-corner row-divider bottom-col-divider bottom-right-corner))
    "\n"))

(module+ test
 (check-equal? 
  (table->string '(("A" "B" "C") ("1" "2" "3")))
  (string-join 
  '("┌─┬─┬─┐"
    "│A│B│C│"
    "├─┼─┼─┤"
    "│1│2│3│"
    "└─┴─┴─┘")
   "\n")))
