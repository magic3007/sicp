#lang racket


#|
Closure Implementation for Data-driving Programming
|#

(define (apply-generic op . args)
  (apply (car args) (cons op (cdr args))))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond [(eq? op 'real-part) x]
          [(eq? op 'imag-part) y]
          [(eq? op 'magnitude)
           (sqrt (+ (sqr x) (sqr y)))]
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)


(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(magnitude (make-from-real-imag 3 4))