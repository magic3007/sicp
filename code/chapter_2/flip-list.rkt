#lang racket

(define (flip lst)
  (if (null? lst)
      '()
      (append (flip (cdr lst)) (list (car lst)))))

(define (readcase lst)
  ; (displayln lst)
  (if (eq? lst eof)
      (void)
      (begin
        (displayln (flip lst))
        (readcase (read)))))
  
(readcase (read))