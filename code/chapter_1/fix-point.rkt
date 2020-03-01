#lang racket

(define (average a b)
  (/ (+ a b) 2))

(define threshold 1e-6)

(define (close-enough? a b)
  (< (abs (- a b)) threshold))

(define (cube x)
  (* x x x))

; average dump
(define (fixed-point f first-guess)
  ; f(x) and (x+f(x))/2 share the same fixed point.
  (define (average-dump f)(lambda (x) (average x (f x))))
  (define (try f guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try f next))))
  (try (average-dump f) first-guess))

(fixed-point cos 1.0)
(fixed-point (lambda (x) (+ (sin x) (cos x))) 1.0)
(fixed-point (lambda (x) (/ 4 x)) 1)