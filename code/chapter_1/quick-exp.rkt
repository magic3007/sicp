#lang racket


(define (foo cur tmp a n)
  (cond ( (= a 0) 0)
        ( (= n 0) cur)
        ( else
          (if (= 1 (remainder n 2))
            (foo (* cur tmp) (* tmp tmp) a (quotient n 2))
            (foo cur (* tmp tmp) a (quotient n 2)))
          ))
  )

(define (testcase a n)
    (if (eq? n eof)
        (void)
        (begin (displayln (foo 1 a a n)) (testcase (read) (read)))
    )
)

(testcase (read) (read))

; ======================================================================
; another implementation


(define (fast-exp a n)
  (define (square x) (* x x))
  (define (iter a n result)
    (if (= n 0)
        result
        (if (even? n) 
            (iter (square a) (/ n 2) result)
            (iter (square a) (/ (- n 1) 2) (* a result)))))
  (iter a n 1))