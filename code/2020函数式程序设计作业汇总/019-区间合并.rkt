#lang racket


(define (merge lst rv cur)
  (if (null? lst)
      (append rv `(,cur))
      (let ([a (first lst)]
            [rs (rest lst)])
        (if (< (second cur) (first a))
            (merge rs (append rv `(,cur)) a)
            (merge rs rv `(,(first cur) ,(max (second cur) (second a))))))))

(define (driver)
  (let-values ([(a b) (values (read) (read))])
    (unless (eq? a eof)
      (let* ([c (sort (append a b) < #:key car)])
        (begin
          (displayln (merge (rest c) '() (first c)))
          (driver))))))

(driver)