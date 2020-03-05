#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define (flip-tree x)
  (if (not (list? x))
      x
      (let ([tmp (reverse x)])
        (map flip-tree tmp)))) 

(define (readcase lst)
  ; (displayln lst)
  (if (eq? lst eof)
      (void)
      (begin
        (displayln (flip-tree lst))
        (readcase (read)))))
  
(readcase (read))
