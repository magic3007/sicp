#lang racket

(define (flat-map lst)
  (cond
    [(not (list? lst)) `(,lst)]
    [(null? lst) '()]
    [else (append-map (lambda (x) (flat-map x)) lst)]))
           
(define (driver)
  (let ([lst (read)])
    (unless (eq? lst eof)
      (begin (displayln (flat-map lst)) (driver)))))

(driver)
