#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (testcase a b)
  (if (eq? a eof)
      (void)
      (begin (displayln (gcd a b)) (testcase (read) (read)))))

(testcase (read) (read))