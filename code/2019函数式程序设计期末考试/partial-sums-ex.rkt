#lang racket
(require r5rs)
(define env (scheme-report-environment 5))
(eval '(define (stream-car stream) (car stream)) env)
(eval '(define (stream-cdr stream) (force (cdr stream))) env)
(eval '(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))])) env)

(eval '(define the-empty-stream '()) env)
(eval '(define (stream-null? stream) (null? stream)) env)
         
(eval '(define (stream-ref s n)  ;get the nth item from s. n starts from 
  (if (stream-null? s) the-empty-stream
      (if (= n 0)
          (stream-car s)
          (stream-ref (stream-cdr s) (- n 1)))))
      env)

(eval '(define (display-stream s n) ;display first n items of s
  (if (= n 0)
      (displayln "")
      (begin (display (stream-car s)) (display " ") (display-stream (stream-cdr s) (- n 1)))))
      env)

(eval '(define (partial-sums-ex op s)
  (let* ([lst (prefix s)]
         [op-lst (stream-map (lambda (x) (apply op x)) (stream-cddr lst))])
    (cons-stream (stream-ref s 0) (cons-stream (stream-ref s 1) op-lst))))
env)

(eval '
      (define (prefix s)
        (letrec ([s0 (stream-ref s 0)]
               [s1 (stream-ref s 1)]
               [rs (stream-cddr s)]
               [prepend (lambda (s)
                                (cons-stream
                                 `(,s0 . ,(stream-ref s 0))
                                 (cons-stream
                                  `(,s1 . ,(stream-ref s 1))
                                  (prepend (stream-cddr s)))))])
          (prepend
           (cons-stream '() (cons-stream '() (prefix rs))))))
      env)
                                 
(eval '
      (define (stream-cddr s)
        (stream-cdr (stream-cdr s)))
      env)

(eval '
      (define (stream-map proc . argstreams)
        (cons-stream
         (apply proc (map stream-car argstreams))
         (apply stream-map (cons proc (map stream-cdr argstreams)))))
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