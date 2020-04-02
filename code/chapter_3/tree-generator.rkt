#lang racket


(define (tree->generator tree)
  (letrec ([cc #f]
           [value 'init]
           [generator (lambda ()
                        (let loop ([tree tree])
                          (if (null? tree)
                              'done
                              (let ([t (let/cc k (set! generator (lambda() (k #f))) #t)])
                                (if t
                                    (cc (first tree))
                                    (for-each loop (cdr tree))))))
                        (cc 'done))])
    (lambda (op)
      (match op
        ['value value]
        ['next (set! value (let/cc k (set! cc k) (generator)))
               value]
        [_ (error "unknown operation.")]))))


(define iter (tree->generator `(1 (2 (3 (4)) (5)) (6 (7 (8) (9))))))

(for ([i (range 11)])
  (displayln (iter 'value))
  (displayln (iter 'next)))
