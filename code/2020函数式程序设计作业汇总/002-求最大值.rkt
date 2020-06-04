#lang racket


(define (readlst m)
  (build-list m (lambda (x) (read))))

(define n (read))

(for ([i (in-range n)])
  (displayln (apply max (readlst (read)))))
