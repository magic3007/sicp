#lang racket

(define (C m n t)
  (display t)
  (display " ")
  (unless (= m n)
    (C (add1 m) n (/ (* t (- n m)) (add1 m)))))

(define (pascal n)
  (for ([i (in-range n)])
    (C 0 i 1)
    (newline)))

(define (driver)
  (let ([n (read)])
    (unless (eq? n eof)
      (begin (pascal n) (driver)))))

(driver)