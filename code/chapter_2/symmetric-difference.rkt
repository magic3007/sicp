#lang racket

(define (minus a b)
  (define in? (curryr member b))
  (filter (negate in?) a))

(define (symmetric-diff a b)
  (append (minus a b) (minus b a)))

(define (format x) (sort (remove-duplicates x) <))

(define (readcases)
  (let* ([a (read)]
         [b (read)])
    (if (eq? a eof)
        (void)
        (begin
          (display (format (minus a b)))
          (display (format (symmetric-diff a b)))
          (newline)
          (readcases)))))

(readcases)