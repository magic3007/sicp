#lang racket

 (define (search lst)
    (if (null? lst)
        (list '())
        (let* ([fir (first lst)]
              [tmp (search (cdr lst))]
              [tmp2 (map (lambda (x) (cons fir x)) tmp)])
          (append tmp2 tmp))))

(define (power-set lst n)
  (if (eq? n 0)
      lst
      (let* ([tmp (search lst)])
        (power-set tmp (sub1 n)))))
 
(define (readcases lst n)
  (if (eq? lst eof)
      (void)
      (begin
        (displayln (power-set  (remove-duplicates (sort lst <)) n))
        (readcases (read) (read)))))

(readcases (read) (read))