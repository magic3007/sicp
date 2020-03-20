#lang racket

(define (grow lst s deep root)
  (if (eq? deep 0)
      `(,root)
      (let* ([temp (* root s)]
             [son (map (curry + temp) lst)]
             [remain (map (curry grow lst s (sub1 deep)) son)])
        (cons root remain))))
                   
(define (readcases)
  (let-values ([ (lst s deep) (values (read) (read) (read))])
    (if (eq? lst eof)
        (void)
        (begin (displayln (grow lst s deep 0))
          (readcases)))))

(readcases)