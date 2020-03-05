#lang racket

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                (map (lambda (p) (cons x p))
                     (permutations (remove x s))))
              s)))

(define (filter pred seq)
  (cond
    ((null? seq) '())
    ((pred (car seq))
     (cons (car seq)
           (filter pred (cdr seq))))
    (else
     (filter pred (cdr seq)))))

(define (cartesian-product . seqs)
  (if (null? seqs)
      (list '())
      (let ((tmp (apply cartesian-product (cdr seqs))))
        (flatmap (lambda (x)
                   (map (lambda (e) (cons x e))
                        tmp))
                 (car seqs)))))

(define (eight-queen? lst)
  (let ((pairs (filter
                (lambda (x) (not (= (car x) (cadr x))))
                (cartesian-product (enumerate-interval 0 7) (enumerate-interval 0 7)))))
    (accumulate (lambda (x y) (and x y))
                #t
                (map (lambda (x)
                       (not
                        (= (abs (- (car x) (cadr x)))
                           (abs (- (list-ref lst (car x)) (list-ref lst (cadr x)))))))
                     pairs))))

(define (print-list lst)
  (if (null? lst)
      (newline)
      (begin
        (display (car lst))
        (print-list (cdr lst)))))

(define (readcase n)
  (define (iter ans n)
       (if (= n 0)
           (void)
           (begin
             (print-list (list-ref ans (- (read) 1)))
             (iter ans (- n 1)))))
  (let ((ans (filter eight-queen? (permutations (enumerate-interval 1 8)))))
     (iter ans n)))

(readcase (read))