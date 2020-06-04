#lang racket

(define (reverse-tree lst)
  (cond
    [(not (list? lst)) lst]
    [else (reverse (map (lambda (x) (reverse-tree x)) lst))]))
           
(define (driver)
  (let ([lst (read)])
    (unless (eq? lst eof)
      (begin (displayln (reverse-tree lst)) (driver)))))

(driver)
