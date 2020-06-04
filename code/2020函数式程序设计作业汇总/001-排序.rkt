#lang racket


(define (readlst)
  (let ([a (read)])
    (if (eq? a eof)
        '()
        (cons a (readlst)))))

(for-each (lambda (x) (display x) (display " "))
          (sort (remove-duplicates (readlst)) <))