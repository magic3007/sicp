#lang racket

(define n (read))
(define k (read))
(define g (* k 2))
(define S (make-vector g #f))
(define q (make-vector g))
(define h -1)
(define r 1)

(define (GS x s S)
  (unless (or (< x 0) (>= x g) (vector-ref S x))
      (begin
        (vector-set! S x #t)
        (vector-set! q r (list x (+ s 1)))
        (set! r (+ r 1)))))

(define (bfs)
  (set! h (+ h 1))
  (let ((t (vector-ref q h)))
    (let ((x (car t)) (s (cadr t)))
      (if (= x k) s (begin
                      (GS (- x 1) s S)
                      (GS (+ x 1) s S)
                      (GS (* x 2) s S)
                      (bfs))))))
(displayln (if (>= n k) (- n k) (begin (vector-set! S n #t) (vector-set! q 0 (list n 0)) (bfs))))
