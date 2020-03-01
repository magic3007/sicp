#lang racket

; iteration method
(define (fast-exp a n)
  (define (square x) (* x x))
  (define (iter a n result)
    (if (= n 0)
        result
        (if (even? n) 
            (iter (square a) (quotient n 2) result)
            (iter (square a) (quotient n 2) (* a result)))))
  (iter a n 1))

(define (testcase a n)
    (if (eq? n eof)
        (void)
        (begin (displayln (fast-exp a n)) (testcase (read) (read)))
    )
)

(testcase (read) (read))
