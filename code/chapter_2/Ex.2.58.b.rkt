#lang racket

(define variable? symbol?)
(define same-variable? eq?)

(define sum? (curry member '+))
(define product? (curry member '*))
 
(define (wrap e) (if (list? e) e (list e)))
(define (unwrap e) (if (and (list? e) (null? (cdr e))) (unwrap (car e)) e))

(define (make-sum x y)
  (cond [(eq? 0 x) y]
        [(eq? 0 y) x]
        [else (append (wrap x) (list '+) (wrap y))]))

(define (make-product x y)
  (cond [(or (eq? x 0) (eq? y 0)) 0]
        [(eq? x 1) y]
        [(eq? y 1) x]
        [else (list x '* y)]))

(define (index-of lst v n)
  (if (null? lst)
      (error "wrong format")
      (if (eq? (car lst) v)
          n
          (index-of (cdr lst) v (add1 n)))))

(define (split lst dlt)
   (let* ([index (index-of lst dlt 0)])
     (values (take lst index) (drop lst (add1 index)))))

(define (addend e) (let-values ([(a b) (split e '+)]) (unwrap a)))
(define (augend e) (let-values ([(a b) (split e '+)]) (unwrap b)))
(define (multiplier e) (let-values ([(a b) (split e '*)]) (unwrap a)))
(define (multiplicand e) (let-values ([(a b) (split e '*)]) (unwrap b)))

(define (deriv exp var)
  (cond ((number? exp ) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum 
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else 
         (error "wrong format"))))

(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (display (deriv a 'x)) (newline) (myloop)))))


(myloop)
