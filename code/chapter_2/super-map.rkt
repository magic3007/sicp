#lang racket

; http://lisp.test.openjudge.org/2019hw3/2/
; super-map过程以一个取n个参数的过程p和n个表为参数,将过程p应用与所有表
; 的第一个元素,而后应用于所有表的第二个元素....如此下去,然后将所有应用的结
; 果收集成一张表返回
(define (exit) #f)
(define (map op lst)
  (if (null? lst)
      '()
      (cons (op (car lst))
            (map op (cdr lst)))))
  
(define (super-map op . w)
  (if (= 0 (length (list-ref w 0)))
      '()
      (let ((tmp1 (map car w))
            (tmp2 (cons op (map cdr w))))
          (cons (apply op tmp1)
            (apply super-map tmp2)))))

(define (myloop)
  (let ((a (read))
        (b (read))
        (c (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (super-map + a b c)) 
               (displayln (super-map (lambda (x y) (+ x (* 2 y) )) a b ))
               (myloop)))))
(myloop)