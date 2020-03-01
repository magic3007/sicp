#lang racket

; http://lisp.test.openjudge.org/2019hw3/1/

(define (cons x y)
  (lambda (m) (m x y)))

(define (car p)
  (let ((f (lambda (x y) x)))
    (p f)))

(define (cdr p)
  (let ((f (lambda (x y) y)))
    (p f)))   

(displayln (car (cons 1 2)))
(displayln (cdr  (cons 1 2)))
(displayln (car (cons 100 (list 2 3))))
(displayln (cdr (cons 13 (list 1 66 7 3))))
(define z (cons 100 (cons 200 (cons 300 (cons 400 '())))))
(displayln (car (cdr z)))
(displayln (car (cdr (cdr z))))
(displayln (cdr (cons 1 '())))