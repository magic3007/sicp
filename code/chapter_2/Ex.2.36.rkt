#lang racket

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? seqs)
      '()
      (cons (accumulate op init
                        (map car seqs))
            (accumulate-n op init
                          (let ((tmp (map cdr seqs)))
                            (if (null? (car tmp))
                                '()
                                tmp))))))
                        
(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(display (accumulate-n + 10 s)) (newline)
(display (accumulate-n * 1 s)) (newline)
(display (accumulate-n cons '() s)) (newline)
(display "******") (newline)
(define (myloop)
  (let ((lst (read)))
    (if (eq? lst eof)
        (void)
        (begin (display (accumulate-n + 0 lst)) (newline)
               (display (accumulate-n cons '(a) lst)) (newline)
               (myloop)))))

             

(myloop)
