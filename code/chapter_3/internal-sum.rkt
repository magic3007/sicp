#lang racket

(define maxn 1000000000)

(define (lowbit x) (bitwise-and x (- x)))

(define (ask hs x)
  (if (= x 0) 0
      (+ (hash-ref hs x 0) (ask hs (- x (lowbit x))))))

(define (update hs x val)
  (when (<= x maxn)
    (begin (hash-set*! hs x (+ (hash-ref hs x 0) val))
           (update hs (+ x (lowbit x)) val))))

(define a (make-hash))
(define b (make-hash))

(define (query x)
  (- (* (add1 x) (ask a x)) (ask b x)))

(define (loop m)
  (unless (eq? m 0)
    (let ([op (read)])
      (match op
        [1 (let-values ([(l r val) (values (read) (add1 (read)) (read))])
             (update a l val)
             (update a r (- val))
             (update b l (* l val))
             (update b r (* r (- val))))]
        [0 (let-values ([(l r) (values (sub1 (read)) (read))])
             (displayln (- (query r) (query l))))])
      (loop (sub1 m)))))

(loop (read))