#lang racket

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define (tri-num-list n s)
  (define (filter predicate sequence)
  (cond
    ((null? sequence) '())
    ((predicate (car sequence))
     (cons (car sequence)
           (filter predicate (cdr sequence))))
    (else (filter predicate (cdr sequence)))))
  (flatmap (lambda (i)
             (filter (lambda (triple)
                     (and (< (cadr triple) (caddr triple)) (<= (caddr triple) n)))
             (map (lambda (j)
                        (let ((k (- s (+ i j))))
                              (list i j k)))        
                        (enumerate-interval (+ i 1) n))))
           (enumerate-interval 1 (- n 2))))

(define (myloop)
  (let ((n (read))
        (s (read)))
    (if (eq? n eof)
        (void)
        (begin (display (tri-num-list n s)) (newline) (myloop)))))

(myloop)