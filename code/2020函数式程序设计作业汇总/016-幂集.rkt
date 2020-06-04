#lang racket

(define (power-set s)
  (if (null? s)
      `(())
      (let* ([fi (first s)]
             [a (power-set (rest s))]
             [b (map (curry cons fi) a)])
        (append b a))))

(define (driver)
  (let-values ([(lst n)
                (values (sort (remove-duplicates (read)) <)
                        (read))])
    (unless (eq? lst eof)
      (begin
        (unless (null? lst)
        (for ([i (in-range n)])
          (set! lst (power-set lst))))
        (displayln lst)
        (driver)))))

(driver)