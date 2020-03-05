#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (flat-map x)
  (if (not (list? x))
      (list x)
      (flatmap flat-map x)))

(define (readcase lst)
  ; (displayln lst)
  (if (eq? lst eof)
      (void)
      (begin
        (displayln (flat-map lst))
        (readcase (read)))))
  
(readcase (read))