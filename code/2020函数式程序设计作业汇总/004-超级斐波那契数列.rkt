#lang racket

(define (f n f1 f2 f3 f4 f5)
  (if (= n 0)
      f1
      (f (sub1 n)
         (+ f1
            (* f2 4)
            (* f3 5)
            (- (* 2 f4 f4))
            (* f5 f5 f5))
         f1
         f2
         f3
         f4)))

(define (driver)
  (let ([n (read)])
    (unless (eq? n eof)
      (begin
        (if (<= n 4)
            (displayln 1)
            (displayln (f (- n 4) 1 1 1 1 1)))
        (driver)))))

(driver)