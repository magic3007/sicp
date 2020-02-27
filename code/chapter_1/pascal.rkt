#lang racket

(define (print list)
    (if(null? list)
        (newline)
        (begin (display (car list)) (display " ") (print (cdr list)))
    )
)


(define (scan list res)
    (cond ((= (length list) 0) (begin (print res) res))
          ((= (length list) 1) (scan '() (cons 1 res)))
          (else
            (scan (cdr list)
                    (cons (+ (car list) (cadr list)) res)
                )
          )
    )
)


(define (pascal n list)
    (if (eq? n 0)
        (void)
        (pascal (- n 1) (scan list '(1)))
    )
)

(define (testcase n)
    (if (eq? n eof)
        (void)
        (begin (pascal n '()) (testcase (read)))
    )
)

(testcase (read))