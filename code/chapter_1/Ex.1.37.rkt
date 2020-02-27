#lang racket

(define (cont-frac-iter N D k)
  (define (cont-frac-internal N D k now)
    (if (> now k)
        0
        (let ((temp (cont-frac-internal N D k (+ now 1))))
        (/ (N now) (+ (D now) temp)))
    )
 )
  (cont-frac-internal N D k 1)
  )

  
(cont-frac-iter (lambda (x) x) 
           (lambda (x) 1.0)
           30)
 
(cont-frac-iter (lambda (x) (* 2 x))
           (lambda (x) (* 1.0 x))
           30)

(display "********") (newline)
(define (myloop)
  (let ((k (read)))
    (if (eq? k eof)
        (void)
        (begin (display (cont-frac-iter (lambda (x) 1.0) (lambda (x) 1.0) k)) 
(newline) (myloop)))))

(myloop)    