#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '
 (define (count-pairs x)
   (let ([visited `()])
     (define (count x)
       (cond [(not (pair? x)) 0]
             [(memq x visited) 0]
             [else
              (set! visited (cons x visited))
              (+ 1 (count (car x)) (count (cdr x)))]))
     (count x)))
env)

(define (myloop)
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin (displayln (eval-codes codes (void))) (myloop)))))


(myloop)