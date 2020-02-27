#lang racket

; http://lisp.test.openjudge.org/2020hw2/1/
; 
; 超级斐波那契数列f(n)如下定义:
; 
; f(n) = 1   (当0<=n<=4)
; 
; f(n) = f(n-1) + 4 * f(n-2) + 5 * f(n-3) - 2 * f(n-4)*f(n-4) +  f(n-5)*f(n-5)*f(n-5)           (当n>4)
; 
; 
; 给定n，求f(n)

(define (foo v0 v1 v2 v3 v4 n)
  (define (calc v0 v1 v2 v3 v4)
    (+ v4(
     + (* 4 v3)(
     + (* 5 v2)(
     + (* -2 (* v1 v1))(
     + (* v0 (* v0 v0)) 0 )
    )))))
  
  (if (<= n 4)
      v4
      (foo v1 v2 v3 v4 (calc v0 v1 v2 v3 v4) (- n 1))
  )
)

(define (testcase n)
    (if (eq? n eof)
        (void)
        (begin (displayln (foo 1 1 1 1 1 n)) (testcase (read)))
    )
)

(testcase (read))