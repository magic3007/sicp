#lang racket

(define variable? symbol?)
(define same-variable? eq?)
(define sum? (curry member '+))
(define product? (curry member '*))
(define (wrap e) (if (list? e) e `(,e)))
(define (make-sum x y)
  (cond [(eq? 0 x) y]
        [(eq? 0 y) x]
        [else (append (wrap x) '(+) (wrap y))]))
(define (make-product x y)
  (cond [(or (eq? x 0) (eq? y 0)) 0]
        [(eq? x 1) y]
        [(eq? y 1) x]
        [else `(,x * ,y)]))
(define (split lst dlt)
  (if (eq? (car lst) dlt) (values '() (cdr lst))
      (let-values ([(a b) (split (cdr lst) dlt)])
        (values (cons (car lst) a) b))))
(define (unwrap e) (if (and (list? e) (null? (cdr e))) (unwrap (car e)) e))
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