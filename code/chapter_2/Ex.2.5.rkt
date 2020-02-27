#lang racket

; http://lisp.test.openjudge.org/2020hw2/6/
;
; [Description]
; Show that we can represent pairs of nonnegative integers using only numbers and arithmetic
; operations if we represent the pair a and b as the integer that is the product 2a 3b. Give the
; corresponding definitions of the procedures cons, car, and cdr.

(define (log x y)
 (if (> y x) 0
  (+ (log (quotient x y) y) 1)
 )
)

(define (car x)
  (if (= 0 (remainder x 3))
      (car (quotient x 3))
      (log x 2)
  ))

(define (cdr x)
  (if (= 0 (remainder x 2))
      (cdr (quotient x 2))
      (log x 3)
  ))

(define (fast-exp a n)
  (define (square x) (* x x))
  (define (iter a n result)
    (if (= n 0)
        result
        (if (even? n) 
            (iter (square a) (/ n 2) result)
            (iter (square a) (/ (- n 1) 2) (* a result)))))
  (iter a n 1))
  
(define (cons a b)
  (* (fast-exp 2 a) (fast-exp 3 b)))

(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (display (car (cons a b)))
               (display " ")
               (display (cdr (cons a b)))
               (newline) 
               (myloop)))))

(myloop)