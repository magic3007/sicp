#lang racket

(define (average a b)
  (/ (+ a b) 2))

(define threshold 1e-6)

(define (close-enough? a b)
  (< (abs (- a b)) threshold))

(define (cube x)
  (* x x x))

; fixed-point: average dump
(define (fixed-point f first-guess)
  ; f(x) and (x+f(x))/2 share the same fixed point.
  (define (average-dump f)(lambda (x) (average x (f x))))
  (define (try f guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try f next))))
  (try (average-dump f) first-guess))


; zero-point: newton-transform -> fixed-point
(define (newtons-method g guess)
  (define dx 1e-5)
  (define (deriv g)
    (lambda (x)
      (/ (- (g (+ x dx)) (g x))
         dx)))
  (define (newton-transform g)
    (lambda (x)
      (- x (/ (g x) ((deriv g) x)))))
  (fixed-point (newton-transform g) guess))

(define (sqrt_fixed-point x)
  (fixed-point (lambda (y) (/ x y)) 1.0))

(define (sqrt_newton x)
  (newtons-method (lambda (y) ( - (sqr y) x)) 1.0))

(sqrt_fixed-point 16)
(sqrt_newton 16)

