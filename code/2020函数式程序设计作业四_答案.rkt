****1: 幂集
by 1600012986
#lang racket

(define (powerset lst)
  (if (empty? lst)
      (list empty)
      (let ([next (powerset (cdr lst))])
        (append
         (map (lambda (x) (cons (car lst) x)) next)
         next))))

(define (nf f n val)
  (if (= n 0)
      val
      (nf f (- n 1) (f val))))

(define (run)
  (define lst (read))
  (define cnt (read))
  (if (eq? lst eof)
      (void)
      (begin
       (displayln (nf powerset cnt (sort (remove-duplicates lst) <)))
       (run))))

(run)

by 1800012789
#lang racket

(define (unique l)
  (reverse (set->list (list->set l))))

(define (powerset S)
  (if (empty? S)
      (list '())
      (let ((rst (powerset (cdr S))))
        (append (map (lambda (x) (cons (car S) x)) rst) rst))))

(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (lambda (x)
        (let ((rf (repeated f (- n 1))))
          (f (rf x))))))

(define (main S n)
  (if (eq? S eof)
      (void)
      (begin (displayln ((repeated powerset n) (unique S))) (main (read) (read)))))

(main (read) (read))

by 1700012751
#lang racket

 (define (search lst)
    (if (null? lst)
        (list '())
        (let* ([fir (first lst)]
              [tmp (search (cdr lst))]
              [tmp2 (map (lambda (x) (cons fir x)) tmp)])
          (append tmp2 tmp))))

(define (power-set lst n)
  (if (eq? n 0)
      lst
      (let* ([tmp (search lst)])
        (power-set tmp (sub1 n)))))
 
(define (readcases lst n)
  (if (eq? lst eof)
      (void)
      (begin
        (displayln (power-set  (remove-duplicates (sort lst <)) n))
        (readcases (read) (read)))))

(readcases (read) (read))

by 1800012751
#lang racket
(define (P lst)
  (if (null? lst)
      (list '())
      (append (map (lambda (x) (cons (car lst) x)) (P (cdr lst))) (P (cdr lst)))))  

(define (Pn lst n)
  (if (= n 0)
      (displayln lst)
      (Pn (P lst) (- n 1))))

(define (Sort lst)
  (cond ((null? lst) '())
        (else (append
               (Sort (filter (lambda (x) (< x (car lst))) (cdr lst)))
               (list (car lst))
               (Sort (filter (lambda (x) (> x (car lst))) (cdr lst)))))))


(define (Read)
  (let ((lst (read))
        (n (read)))
    (if (eq? lst eof)
        (void)
        (begin (Pn (Sort lst) n) (Read)))))

(Read)

****2: pipeline
by 1800013002
#lang racket
(define (pipeline operand . ops)
  (if (null? ops) operand (apply pipeline ((car ops) operand) (cdr ops))))
(define (fib n) ;get the nth item of Fibonacci
  (define (helper a b i)
    (if (> i n)
        b
        (helper b (+ a b) (+ i 1))))
  (if (= n 0)
      0
      (helper 0 1 2)))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))


(define (even-fibs n)
  (pipeline (enumerate-interval 1 n)
            (lambda (lst) (map fib lst))
            (lambda (lst) (filter even? lst))
            (lambda (lst) (accumulate cons '() lst))))
(display (even-fibs 9)) (newline)
(display (even-fibs 19)) (newline)

(display "******") (newline)
(define (square x) (* x x))
(define (inc x) (+ x 1))


(define (f1 lst)
  (if (not (pair? (car lst)))
      (pipeline lst
                (lambda (x) (map square x))
                (lambda (x) (map inc x))
                (lambda (x) (map inc x))
                (lambda (x) (map inc x)))
      (pipeline lst
                (lambda (x) (map car x))
                (lambda (x) (map inc x))
                (lambda (x) (map square x))
                (lambda (x) (map inc x))
                (lambda (x) (map inc x)))))

(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (display (f1 a)) (newline)
               (myloop)))))

(myloop)

by 1800012802
#lang racket
(define (pipeline operand . ops)
  (if (null? ops)operand
      (apply pipeline ((car ops) operand) (cdr ops))
  ))
(define (fib n) ;get the nth item of Fibonacci
  (define (helper a b i)
    (if (> i n)
        b
        (helper b (+ a b) (+ i 1))))
  (if (= n 0)
      0
      (helper 0 1 2)))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))


(define (even-fibs n)
  (pipeline (enumerate-interval 1 n)
            (lambda (lst) (map fib lst))
            (lambda (lst) (filter even? lst))
            (lambda (lst) (accumulate cons '() lst))))
(display (even-fibs 9)) (newline)
(display (even-fibs 19)) (newline)

(display "******") (newline)
(define (square x) (* x x))
(define (inc x) (+ x 1))


(define (f1 lst)
  (if (not (pair? (car lst)))
      (pipeline lst
                (lambda (x) (map square x))
                (lambda (x) (map inc x))
                (lambda (x) (map inc x))
                (lambda (x) (map inc x)))
      (pipeline lst
                (lambda (x) (map car x))
                (lambda (x) (map inc x))
                (lambda (x) (map square x))
                (lambda (x) (map inc x))
                (lambda (x) (map inc x)))))

(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (display (f1 a)) (newline)
               (myloop)))))

(myloop)

by 1700013005
#lang racket
(define (pipeline operand . ops)
  (if (null? ops)
      operand
      (apply pipeline ((car ops) operand) (cdr ops))))
(define (fib n) ;get the nth item of Fibonacci
  (define (helper a b i)
    (if (> i n)
        b
        (helper b (+ a b) (+ i 1))))
  (if (= n 0)
      0
      (helper 0 1 2)))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))


(define (even-fibs n)
  (pipeline (enumerate-interval 1 n)
            (lambda (lst) (map fib lst))
            (lambda (lst) (filter even? lst))
            (lambda (lst) (accumulate cons '() lst))))
(display (even-fibs 9)) (newline)
(display (even-fibs 19)) (newline)

(display "******") (newline)
(define (square x) (* x x))
(define (inc x) (+ x 1))


(define (f1 lst)
  (if (not (pair? (car lst)))
      (pipeline lst
                (lambda (x) (map square x))
                (lambda (x) (map inc x))
                (lambda (x) (map inc x))
                (lambda (x) (map inc x)))
      (pipeline lst
                (lambda (x) (map car x))
                (lambda (x) (map inc x))
                (lambda (x) (map square x))
                (lambda (x) (map inc x))
                (lambda (x) (map inc x)))))

(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (display (f1 a)) (newline)
               (myloop)))))

(myloop)

by 1700012966
#lang racket
(define (pipeline operand . ops)
  (if (null? ops)
      operand
      (apply pipeline ((car ops) operand) (cdr ops))))
(define (fib n) ;get the nth item of Fibonacci
  (define (helper a b i)
    (if (> i n)
        b
        (helper b (+ a b) (+ i 1))))
  (if (= n 0)
      0
      (helper 0 1 2)))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))


(define (even-fibs n)
  (pipeline (enumerate-interval 1 n)
            (lambda (lst) (map fib lst))
            (lambda (lst) (filter even? lst))
            (lambda (lst) (accumulate cons '() lst))))
(display (even-fibs 9)) (newline)
(display (even-fibs 19)) (newline)

(display "******") (newline)
(define (square x) (* x x))
(define (inc x) (+ x 1))


(define (f1 lst)
  (if (not (pair? (car lst)))
      (pipeline lst
                (lambda (x) (map square x))
                (lambda (x) (map inc x))
                (lambda (x) (map inc x))
                (lambda (x) (map inc x)))
      (pipeline lst
                (lambda (x) (map car x))
                (lambda (x) (map inc x))
                (lambda (x) (map square x))
                (lambda (x) (map inc x))
                (lambda (x) (map inc x)))))

(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (display (f1 a)) (newline)
               (myloop)))))

(myloop)

****3: 怎么能够这么求和
by 1700012846(yeongmin)
#lang racket
(define (mysum x)
  (lambda l
    (if (null? l) x
        (mysum (+ x (car l))))))
(define (f s n)
  (define (exec g i) ; call g for i times
    (if (= i 1)
        (g)
        (begin (g) (exec g (- i 1)))))
    
  (define (iter k ls)
    (if (null? ls)
        (exec k n) ;call k for n times. same effect as (k). just to prevent cheating
        (iter (k (car ls)) (cdr ls))))
  (iter (mysum (car s)) (cdr s)))

(define k ((((mysum 1) 2) 3) 4))
(define k2 ((mysum 10) 20))

(displayln (k))
(displayln (k2))

(define (myloop)
  (let ((s (read)))
    (if (eq? s eof)
        (void)
        (begin (displayln (f s (car s)))
               (myloop)))))
(myloop)

by 1600012986
#lang racket
(define (mysum x)
  (lambda val
    (if (empty? val)
        x
        (mysum (+ x (car val))))))
(define (f s n)
  (define (exec g i) ; call g for i times
    (if (= i 1)
        (g)
        (begin (g) (exec g (- i 1)))))
    
  (define (iter k ls)
    (if (null? ls)
        (exec k n) ;call k for n times. same effect as (k). just to prevent cheating
        (iter (k (car ls)) (cdr ls))))
  (iter (mysum (car s)) (cdr s)))

(define k ((((mysum 1) 2) 3) 4))
(define k2 ((mysum 10) 20))

(displayln (k))
(displayln (k2))

(define (myloop)
  (let ((s (read)))
    (if (eq? s eof)
        (void)
        (begin (displayln (f s (car s)))
               (myloop)))))
(myloop)

by 1800013090
#lang racket
(define (mysum x)
  (lambda y
    (cond
      ((null? y) x)
      (else (mysum (+ x (car y)))))))
(define (f s n)
  (define (exec g i) ; call g for i times
    (if (= i 1)
        (g)
        (begin (g) (exec g (- i 1)))))
    
  (define (iter k ls)
    (if (null? ls)
        (exec k n) ;call k for n times. same effect as (k). just to prevent cheating
        (iter (k (car ls)) (cdr ls))))
  (iter (mysum (car s)) (cdr s)))

(define k ((((mysum 1) 2) 3) 4))
(define k2 ((mysum 10) 20))

(displayln (k))
(displayln (k2))

(define (myloop)
  (let ((s (read)))
    (if (eq? s eof)
        (void)
        (begin (displayln (f s (car s)))
               (myloop)))))
(myloop)

by 1800013069
#lang racket
(define (mysum x)
  (define (tmp . y)
    (if (empty? y) x
        (mysum (+ x (car y)))))
  tmp)
(define (f s n)
  (define (exec g i) ; call g for i times
    (if (= i 1)
        (g)
        (begin (g) (exec g (- i 1)))))
    
  (define (iter k ls)
    (if (null? ls)
        (exec k n) ;call k for n times. same effect as (k). just to prevent cheating
        (iter (k (car ls)) (cdr ls))))
  (iter (mysum (car s)) (cdr s)))

(define k ((((mysum 1) 2) 3) 4))
(define k2 ((mysum 10) 20))

(displayln (k))
(displayln (k2))

(define (myloop)
  (let ((s (read)))
    (if (eq? s eof)
        (void)
        (begin (displayln (f s (car s)))
               (myloop)))))
(myloop)

****4: 区间合并
by 1700012751
#lang racket

(define (merge lst res cur)
  (if (null? lst)
      (append res (list cur))
      (let ([a (first lst)])
        (if (< (second cur) (first a))
            (merge (cdr lst) (append res (list cur)) a)
            (merge (cdr lst) res (list (first cur) (max (second cur) (second a))))))))
            
(define (readcases)
  (let-values ([(a b) (values (read) (read))])
    (if (eq? a eof)
        (void)
        (let ([c (sort (append a b) < #:key car)])
          (begin
            (displayln (merge (cdr c) '() (first c)))
            (readcases))))))
               
(readcases)


by 1800012802
#lang racket
(define (merge a b)
  (cond
    ((null? a) b)
    ((null? b) a)
    (else
     (if (< (car (car a)) (car (car b)))
         (cons (car a) (merge (cdr a) b))
         (cons (car b) (merge a (cdr b)))))))
(define (reduce a)
  (cond
    ((null? a) a)
    ((null? (cdr a)) a)
    (else
     (if (< (cadr (car a)) (car (cadr a)))
         (cons (car a) (reduce (cdr a)))
         (reduce (cons (list (car (car a)) (max (cadr (car a)) (cadr (cadr a)))) (cddr a)))))))
(define (work a b)
  (if (eq? a eof)
      (void)
      (begin
        (displayln (reduce (merge a b)))
        (work (read) (read)))))
(work (read) (read))

by 1800012901
#lang racket
(define (merge a b)
  (cond
    ((null? a) b)
    ((null? b) a)
    (else
     (if (< (car (car a)) (car (car b)))
         (cons (car a) (merge (cdr a) b))
         (cons (car b) (merge a (cdr b)))))))
(define (reduce a)
  (cond
    ((null? a) a)
    ((null? (cdr a)) a)
    (else
     (if (< (cadr (car a)) (car (cadr a)))
         (cons (car a) (reduce (cdr a)))
         (reduce (cons (list (car (car a)) (max (cadr (car a)) (cadr (cadr a)))) (cddr a)))))))
(define (work a b)
  (if (eq? a eof)
      (void)
      (begin
        (displayln (reduce (merge a b)))
        (work (read) (read)))))
(work (read) (read))


by 1800012789
#lang racket

(define (le? a1 b1) (< (car a1) (car b1)))

(define (merge ans a b)
  (define (helper l itv)
    (cond
      [(empty? l) (list itv)]
      [(<= (car itv) (cadar l))
       (cons (list (caar l) (max (cadr itv) (cadar l)))
             (cdr l))]
      [else (cons itv l)]))
  (cond
    [(and (empty? a) (empty? b)) ans]
    [(or (empty? b) (and (not (empty? a)) (le? (car a) (car b))))
     (merge (helper ans (car a)) (cdr a) b)]
    [else (merge (helper ans (car b)) a (cdr b))]))

(define (main a b)
  (if (eq? a eof)
      (void)
      (begin (displayln (reverse (merge '() a b))) (main (read) (read)))))

(main (read) (read))


****5: SICP课本练习2.58 (b) 符号求导
by 1700012751
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

by 1800011789
#lang racket
(define (variable? x) (symbol? x))
(define (same-variable? a b) (and (variable? a) (variable? b) (eq? a b)))
(define (unflat x) (if (list? x) x (list x)))
(define (make-sum a b)
    (cond
        ((and (number? a) (number? b)) (+ a b))
        ((and (number? a) (= a 0)) b)
        ((and (number? b) (= b 0)) a)
        (else (append (unflat a) '(+) (unflat b)))
    )
)
(define (make-product a b) 
    (cond
        ((and (number? a) (number? b)) (* a b))
        ((and (number? a) (= a 0)) 0)
        ((and (number? b) (= b 0)) 0)
        ((and (number? a) (= a 1)) b)
        ((and (number? b) (= b 1)) a)
        (else (list a '* b))
    )
)
(define (make-tree l exp)
    (cond
        ((null? exp) (list (cadr l) (car l) (cddr l)))
        ((eq? (car exp) '+) (list '+ l (cdr exp)))
        (else (make-tree (append l (list (car exp))) (cdr exp)))
    )
)
(define (flat li)
    (if (null? (cdr li)) (car li) li)
)
(define (sum? x) (and (pair? x) (eq? (car (make-tree '() x)) '+)))
(define (multiplier x) (car x))
(define (multiplicand x) (if (null? (cdddr x)) (caddr x) (cddr x)))
(define (product? x) (and (pair? x) (eq? (car (make-tree '() x)) '*)))
(define (addend x) (flat (cadr (make-tree '() x))))
(define (augend x) (flat (caddr (make-tree '() x))))

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

by 1700012846(yeongmin)
#lang racket
(define (variable? v) (symbol? v))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (unwrap e)
  (if (and (pair? e) (null? (cdr e))) (car e) e))
(define (sum? exp)
  (cond [(not (list? exp)) #f]
        [(null? (cdr exp)) #f]
        [(eq? (cadr exp) '+) #t]
        [else (sum? (cddr exp))]))
(define (make-sum e1 e2)
  (cond [(eq? e1 0) e2]
        [(eq? e2 0) e1]
        [(list? e2) (if (list? e1)
                        (append e1 (list '+) e2)
                        (append (list e1 '+) e2))]
        [(list? e1) (append e1 (list '+ e2))]
        [else (list e1 '+ e2)]))
(define (addend e)
  (define (helper res e)
    (if (eq? (cadr e) '+) (append res (list (car e)))
        (helper (append res (list (car e) (cadr e))) (cddr e))))
  (unwrap (helper null e)))
(define (augend e)
  (define (helper e)
    (if (eq? (cadr e) '+)
        (cddr e)
        (helper (cddr e))))
  (unwrap (helper e)))
(define (product? exp)
  (let ([e (unwrap exp)])
    (and (list? e) (not (sum? e)))))
(define (make-product e1 e2)
  (cond [(eq? e1 0) 0]
        [(eq? e2 0) 0]
        [(eq? e1 1) e2]
        [(eq? e2 1) e1]
        [else (list e1 '* e2)]))
(define (multiplier e)
  (unwrap (car e)))
(define (multiplicand e)
  (unwrap (cddr e)))
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

by 1600012732
#lang racket
(define (=number? x v)
  (and (number? x) (= x v)))
(define (flat x)
  (if (null? (cdr x))
        (car x)
        x))
(define (find lst v)
  (define (iter lst pos)
    (if (eq? (car lst) v)
        pos
        (iter (cdr lst) (+ pos 1))))
  (iter lst 0))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (let ((l1 (if (pair? a1) a1 (list a1)))
        (l2 (if (pair? a2) a2 (list a2))))
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (append l1 (list '+) l2)))))
(define (make-product m1 m2)
  (let ((l1 (if (or (sum? m1) (not (pair? m1))) (list m1) m1))
        (l2 (if (or (sum? m2) (not (pair? m2))) (list m2) m2)))
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (append l1 (list '*) l2)))))
(define (sum? x)
  (and (pair? x) (not (null? (filter (lambda (t) (eq? t '+)) x)))))
(define (addend s)
  (flat (take s (find s '+))))
(define (augend s)
  (flat (drop s (+ 1 (find s '+)))))
(define (product? x)
  (and (pair? x) (not (sum? x))))
(define (multiplier p)
  (car p))
(define (multiplicand p)
  (flat (cddr p)))
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

