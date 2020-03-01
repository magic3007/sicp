#lang racket

(define (average a b)
  (/ (+ a b) 2))

(define threshold 1e-6)

(define (close-enough? a b)
  (< (abs (- a b)) threshold))

(define (cube x)
  (* x x x))

(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        mid-point
        (let ((test-value (f mid-point)))
          (cond ((positive? test-value)
                 (search f neg-point mid-point))
                ((negative? test-value)
                 (search f mid-point pos-point))
                (else mid-point))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign"  a b)))))

(half-interval-method
 (lambda (x) (- (cube x) (* 2 x) 3))
 1.0
 2.0)