#lang racket
(require r5rs)
(define (exit) #f)
(define env (scheme-report-environment 5))

(eval '(define (last-pair lst)
         (if (null? (cdr lst))
             lst
             (last-pair (cdr lst))))
      env)

(eval '(define (make-cycle lst)
         (set-cdr! (last-pair lst) lst)
         lst)
      env)


(eval '
(define (check-cycle x)
  (let ([ongoing  `()]
        [done `()])
    (define (check x)
      (cond [(not (pair? x)) #f]
            [(memq x ongoing) #t]
            [(memq x done) #f]
            [else
              (set! ongoing (cons x ongoing))
              (let ([res (or (check (car x)) (check (cdr x)))])
                (set! ongoing (cdr ongoing))
                (set! done (cons x done))
                res)]))
    (check x)))
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