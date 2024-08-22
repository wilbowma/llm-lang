(define (draw-table rows)
  (define (get-column-widths rows)
    (map (lambda (col)
           (apply max (map (lambda (row) (string-length (list-ref row col))) rows)))
         (range (length (car rows)))))
  
  (define (draw-divider widths)
    (apply string-append
           (map (lambda (w) (string-append "+-" (make-string w #\-) "-"))
                widths))
    "+")
  
  (define (draw-row row widths)
    (apply string-append
           (map (lambda (cell width)
                  (string-append "| " cell (make-string (- width (string-length cell)) #\space)))
                row widths))
    "|")
  
  (define widths (get-column-widths rows))
  (define divider (draw-divider widths))
  
  (string-append divider "\n"
                 (draw-row (car rows) widths) "\n"
                 divider "\n"
                 (apply string-append
                        (map (lambda (row)
                               (draw-row row widths) "\n")
                             (cdr rows)))
                 divider))
