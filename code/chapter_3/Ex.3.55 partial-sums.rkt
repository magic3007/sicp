#lang racket
;book ex 3.55
(require r5rs)
(define env (scheme-report-environment 5))
(eval '(define (stream-car stream) (car stream)) env)
(eval '(define (stream-cdr stream) (force (cdr stream))) env)
(eval '(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))])) env)

(eval '(define the-empty-stream '()) env)
(eval '(define (stream-null? stream) (null? stream)) env)
         
(eval '(define (stream-ref s n)  ;取 stream里面第 n 项,n从0开始算
  (if (stream-null? s) the-empty-stream
      (if (= n 0)
          (stream-car s)
          (stream-ref (stream-cdr s) (- n 1)))))
      env)


(eval '
 (define (partial-sums op s) ;以s为参数,返回的流是 s0,(op s0 s1),(op s0 s1 s2), ....
   (let* ([lst (prefix s)]
          [op-lst (stream-map (lambda (x) (apply op x)) (stream-cdr lst))])                        
   (cons-stream (stream-car s) op-lst)))
      env)

(eval '
      (define (prefix s)
      (let* ([s0 (stream-car s)]
             [rest (stream-cdr s)])
        (cons-stream `(,s0)
                     (stream-map (lambda (x) `(,s0 . ,x))
                                 (prefix rest)))))
      env)

(eval '
      (define (stream-map proc . argstrams)
        the-empty-stream
        (cons-stream (apply proc (map stream-car argstrams))
                   (apply stream-map (cons proc (map stream-cdr argstrams)))))
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