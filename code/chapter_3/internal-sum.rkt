#lang racket

(define (make-binary-index-tree n)
  (let ([arr (make-hash)])
    (define (lowbit x) (bitwise-and x (- x)))
    (define (ask x)
      (cond [(= x 0) 0]
            [else (+ (hash-ref arr x 0) (ask (- x (lowbit x))))]))
    (define (update x add)
      (cond [(> x n) (void)]
            [else
             (hash-set! arr x (+ hash-ref arr x 0) add)
             (update (+ x (lowbit x)) add)]))
    (define (dispatch op . args)
      (match op
        ['update 
      
(define (readcases)
  (let ([m (read)])
    (define root (newnode lower-bound upper_bound))
    (define (loop m)
      (if (eq? m 0)
          (void)
          (let ([op (read)])
            (match op
              [0 (let-values ([(l r) (values (read) (read))])
                   (displayln (query root l r)))]
              [1 (let-values ([(l r x) (values (read) (read) (read))])
                   (update root l r x))])
            (loop (sub1 m)))))
    (loop m)))

(readcases)