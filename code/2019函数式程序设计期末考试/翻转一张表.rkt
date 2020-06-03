#lang racket

(define (driver)
  (let ([lst (read)])
    (unless (eq? lst eof)
      (begin (displayln (reverse lst)) (driver)))))

(driver)