#lang racket

(define (C m n t)
  (display t)
  (display " ")
  (if (= m n)
      (newline)
      (C (add1 m) n (/ (* t (- n m)) (+ m 1)))))

(define (pascal n)
  (for ([i (in-range n)]) (C 0 i 1)))

(define (driver)
  (let ([n (read)])
    (unless (eq? n eof)
      (begin (pascal n) (driver)))))

(driver)