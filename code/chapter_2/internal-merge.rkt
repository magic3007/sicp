#lang racket

(define (merge lst res cur)
  (if (null? lst)
      (append res (list cur))
      (let ([a (first lst)])
        (if (< (second cur) (first a))
            (merge (cdr lst) (append res (list cur)) a)
            (merge (cdr lst) res (list (first cur) (max (second cur) (second a))))))))
            
(define (readcases)
  (let-values ([(a b) (values (read) (read))])
    (if (eq? a eof)
        (void)
        (let ([c (sort (append a b) < #:key car)])
          (begin
            (displayln (merge (cdr c) '() (first c)))
            (readcases))))))
               
(readcases)
