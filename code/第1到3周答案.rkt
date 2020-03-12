****第一周
---- 求最大值
by 1800013002
#lang racket

(define (print-max n)
  (if (= n 0)
      0
      (max (read) (print-max (- n 1)))
      )
  )

(define (main n)
  (if (= n 0)
      (void)
      (begin (displayln (print-max (read))) (main (- n 1)))
      )
  )

(main (read))

----帕斯卡三角形

by 1800012751

#lang racket
(define (C i m t)
  (display t)
  (if (eq? i m)
      (display "\n")
      (begin (display " ") (C (+ i 1) m (/ (* t (- m i)) i)))))


(define (Print i n)
  (C 1 i 1)
  (if (eq? i n)
      (void)
      (Print (+ i 1) n)))

(define (Read)
  (define n (read))
  (if (eq? n eof)
      (void)
      (begin (Print 1 n) (Read))))

(Read)

by 1800012789

#lang racket
(define (next l)
  (if (= (length l) 1)
      l
      (cons (+ (car l) (cadr l)) (next (cdr l)))
  )
)

(define (print-list l)
  (if (eq? l '())
      (void)
      (begin
        (display (car l)) (display " ")
        (print-list (cdr l))
      )
  )
)

(define (print-line n l)
  (if (= n 0)
      (void)
      (begin
        (print-list l) (newline)
        (print-line (- n 1) (cons 1 (next l)))
      )
  )
)

(define (main n)
  (if (eq? n eof)
      (void)
      (begin
        (print-line n (list 1))
        (main (read))
      )
  )
)

(main (read))

****第二周
----超级斐波那契数
by 1800017781

#lang racket
(define (func x1 x2 x3 x4 x5 cnt n)
  (if (= cnt n)
      x1
      (func x2 x3 x4 x5 (+ (* x1 x1 x1) (* -2 x2 x2) (* 5 x3) (* 4 x4) x5) (+ cnt 1) n)))

(define (main n)
  (if (eq? n eof)
      (void)
      (begin (displayln (func 1 1 1 1 1 0 n)) (main (read)))))

(main (read))

----SICP EX 2.16 必须用迭代完成的快速幂

by 1800011789
#lang racket
(define (fpow ans base n)
  (if (= n 0)
      ans
      (fpow (if (= (modulo n 2) 0) ans (* ans base)) (* base base) (quotient n 2) )
      )
  )
(do ((a (read)(read)) (n (read)(read)))
  ((eq? eof a) (void))
  (displayln (fpow 1 a n))
  )

----SICP课本练习1.43
#lang racket
by 1800013069
(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (db x) (* x 2))

(define (repeated f n)
  (if (<= n 1) f
      (lambda (x) (f ((repeated f (- n 1)) x)))))
((repeated square 2) 5)
((repeated inc 4) 6)
((repeated db 4) 6)

(display "********") (newline)

(define (myloop)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin (display ((repeated square n) 2)) 
               (newline) (myloop)))))

(myloop)

----SICP课本练习1.43

by 1900012959(lny)

#lang racket
(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (db x) (* x 2))

(define (repeated f n)
(if (> n 1) (lambda (x) ((repeated f (- n 1)) (f x))) f))
((repeated square 2) 5)
((repeated inc 4) 6)
((repeated db 4) 6)

(display "********") (newline)

(define (myloop)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin (display ((repeated square n) 2)) 
               (newline) (myloop)))))

(myloop)

----SICP课本练习1.37
by 1800011789
#lang racket
(define (cont-frac-iter N D k)
(do ((ans 0 (/ (N i) (+ (D i) ans)))(i k (- i 1)))
    ((= i 0) ans))
  )
(cont-frac-iter (lambda (x) x) 
           (lambda (x) 1.0)
           30)
 
(cont-frac-iter (lambda (x) (* 2 x))
           (lambda (x) (* 1.0 x))
           30)

(display "********") (newline)
(define (myloop)
  (let ((k (read)))
    (if (eq? k eof)
        (void)
        (begin (display (cont-frac-iter (lambda (x) 1.0) (lambda (x) 1.0) k)) 
(newline) (myloop)))))

(myloop)

----SICP课本练习1.41
by 1700013005
#lang racket
(define (inc x) (+ x 1))
(define (square x ) (* x x))
(define (doubleF f)
(lambda(x)(f(f x))))
((doubleF square) 10)
(define X (doubleF (doubleF doubleF)))
((X inc) 5)
(((doubleF (doubleF (doubleF doubleF))) inc) 5) ;输出261 

(display "********") (newline)
(define (myloop)
  (let ((k (read)))
    (if (eq? k eof)
        (void)
        (begin (display ((X inc) k)) 
               (newline) (myloop)))))

(myloop)
---- SICP课本练习2.5
by 1700013005
#lang racket
(define (R n p i)
  (if (= 0 (remainder n p)) (R (/ n p) p (+ i 1)) i))
(define (car n) (R n 2 0))
(define (cdr n) (R n 3 0))
(define (fast-exp a n)
  (define (square x) (* x x))
  (define (iter a n result)
    (if (= n 0)
        result
        (if (even? n) 
            (iter (square a) (/ n 2) result)
            (iter (square a) (/ (- n 1) 2) (* a result)))))
  (iter a n 1))
  
(define (cons a b)
  (* (fast-exp 2 a) (fast-exp 3 b)))

(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (display (car (cons a b)))
               (display " ")
               (display (cdr (cons a b)))
               (newline) 
               (myloop)))))

(myloop)

****第三周
----翻转一张表
by 1800013069
#lang racket

(define (work lst)
  (if (equal? lst eof) (void)
      (begin (displayln (reverse lst))
             (work (read)))))
(work (read))

----flat-map
by 1700013005
#lang racket
(define (F x)
  (if (list? x)
      (if (null? x)
          x
          (append (F (car x)) (F (cdr x))))
      (list x)))


(define (Test)
  (let ((L (read)))
    (if (eq? L eof)
        (void)
        (begin (displayln (F L)) (Test)))))


----深度反转一棵树
by 1800012789
#lang racket

(define (rev l)
  (cond ((eq? l '()) '())
        ((not (list? l)) l)
        (else (append (rev (cdr l)) (list (rev (car l)))))))

(define (main l)
  (if (eq? l eof)
      (void)
      (begin (displayln (rev l)) (main (read)))))

(main (read))

----排序
by 1700012966

#lang racket
(define (print l)
  (if (null? (cdr l))
      (display (car l))
      (begin (display (car l)) (display " ") (print (cdr l)))))
(define (start lst x)
  (define (sort l a t)
    (if (null? l)
        (append t (list a))
        (if (= a (car l))
            (sort (cdr l) a t)
            (if (< a (car l))
                (append t (cons a l))
                (sort (cdr l) a (append t (list (car l))))))))
  (if (eq? x eof)
      (print lst)
      (start (sort lst x '()) (read))))
(start '() (read))

----自定义car和cdr
by 1800012945
#lang racket
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z) (z (lambda (x y) x)))
(define (cdr z) (z (lambda (x y) y)))
(displayln (car (cons 1 2)))
(displayln (cdr  (cons 1 2)))
(displayln (car (cons 100 (list 2 3))))
(displayln (cdr (cons 13 (list 1 66 7 3))))
(define z (cons 100 (cons 200 (cons 300 (cons 400 '())))))
(displayln (car (cdr z)))
(displayln (car (cdr (cdr z))))
(displayln (cdr (cons 1 '())))

----super-map
by 1800013069

#lang racket
(define (exit) #f)
(define (map op lst)
  (if (null? lst)
      '()
      (cons (op (car lst))
            (map op (cdr lst)))))
  
(define (super-map op . w)
(if (empty? (car w)) '()
      (cons (apply op (map car w)) (apply super-map (cons op (map cdr w))))))
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


