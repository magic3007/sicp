#lang racket
(require r5rs)
(define (exit) #f)
(define env (scheme-report-environment 5))

(eval '
      (define (composel lst)
        (let ([f0 (car lst)]
              (rs (cdr lst)))
          (if (null? rs)
              f0
              (lambda (x) ((composel rs) (f0 x))))))
      env)
(eval '
      (define (acc-func . argf)
        (if (null? argf)
            acc-func
            (let* ([f0 (composel argf)])
              (lambda x
                (if (null? x)
                    f0
                    (apply acc-func (cons f0 x)))))))
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