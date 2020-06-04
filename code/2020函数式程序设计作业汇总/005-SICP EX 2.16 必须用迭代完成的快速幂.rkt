#lang racket

(define (power a n)
  (define (iter a n rv)
    (cond
      [(= n 0) rv]
      [(even? n) (iter (* a a) (/ n 2) rv)]
      [(odd? n) (iter a (sub1 n) (* rv a))]))
  (iter a n 1))

(define (driver)
  (let-values ([(a n) (values (read) (read))])
    (unless (eq? a eof)
      (begin (displayln (power a n)) (driver)))))

(driver)
