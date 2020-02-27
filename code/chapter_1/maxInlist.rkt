#lang racket

(define (maxnum x y) 
    (if (> x y)
     x
     y)
)

(define (readline m cur)
    (if (eq? m 0)
        (displayln cur)
        (readline (- m 1) (maxnum (read) cur))
    )
)

(define (readline-first m cur)
    (readline (- m 1) cur)
)

(define (testcase num)
    (if (eq? num 0)
        (void)
        (begin (readline-first (read) (read)) (testcase (- num 1)))
    )
)

(testcase (read))