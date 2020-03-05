#lang racket
  
(define (print-line lst)
    (for-each (lambda (x) (display x) (display " ")) lst))

(define (filter predicate sequence)
  (cond
    ((null? sequence) '())
    ((predicate (car sequence))
     (cons (car sequence)
           (filter predicate (cdr sequence))))
    (else (filter predicate (cdr sequence)))))

(define (quicksort x)
  (cond
    ((null? x) '())
    (else (append
           (quicksort (filter (lambda (y) (< y (car x)))
                              (cdr x)))
           (list (car x))
           (quicksort (filter (lambda (y) (>= y (car x)))
                              (cdr x)))))))


(define (readline lst)
  (let ([a (read)])
    (if (eq? a eof)
        (print-line (quicksort lst))
        (readline (cons a lst)))))


(readline '())