****1: SICP课本练习 3.17
by 1700013005
#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '
 (define (count-pairs L)
   (define (T x R)
     (if (and (pair? x) (not (memq x R)))
         (T (car x) (T (cdr x) (cons x R)))
         R))
   (length (T L '())))
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

by 1800012789
#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '
 (define (count-pairs l)
   (define (traverse l table)
     (if (and (pair? l) (not (memq l table)))
         (traverse (car l) (traverse (cdr l) (cons l table)))
         table))
   (length (traverse l '())))
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

by 1800012803(Lavintan)
#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '
(define (count-pairs lst)
    (cond ((not (pair? lst)) 0)
          ((eq? 'over (car lst)) 0)
          (else (let ((t (car lst)))
                  (begin(set-car! lst 'over) (+ 1 (count-pairs t) (count-pairs (cdr lst))))))))
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

by 1700012751
#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '
 (define (count-pairs x)
   (let ([visited `()])
     (define (count x)
       (cond [(not (pair? x)) 0]
             [(memq x visited) 0]
             [else
              (set! visited (cons x visited))
              (+ 1 (count (car x)) (count (cdr x)))]))
     (count x)))
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

****2: SICP 课本练习 3.18
by 1800013002
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
(define (check-cycle c)
  (cond
    ((not (pair? c)) #f)
    ((eq? 'VISITED (car c)) #t)
    (else
     (begin
       (set-car! c 'VISITED)
       (check-cycle (cdr c))))))
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

by 1800012803(Lavintan)
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
  (if (not (pair? x))
      #f
      (if (eq? (car x) 'AWSL)
          #t
          (begin
            (set-car! x 'AWSL)
            (check-cycle (cdr x))))))
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

by 1800012789
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
(define (check-cycle l)
  (cond
    ((or (null? l) (not (pair? l))) #f)
    ((eq? (car l) 'vis) #t)
    (else (let ((x (car l)) (y (cdr l)))
            (set-car! l 'vis)
            (check-cycle y)))))
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

by 1600012986
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
(define (check-cycle p)
   (define (iter p entered)
     (if (member p entered)
         #t
         (if (pair? p)
             (or (iter (car p) (cons p entered))
                 (iter (cdr p) (cons p entered)))
             #f)))
   (iter p '()))
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

****3: 走迷宫
by 1800012802
#lang racket
(define M '())
(define s '())
(define d 0)
(define (get m y)
  (if(= y 1)(car m)(get (cdr m)(- y 1))))
(define (A x y m)
  (if(= x 1)(get (car m) y)
     (A (- x 1) y (cdr m))))
(define (mread c)
  (if(= c 0)'()
     (cons (read)(mread (- c 1)))))
(define (Mread r c)
  (if(= r 0)'()
     (cons (mread c) (Mread (- r 1) c))))
(define (dis c)
  (if(equal? c 'W)5000 1))
(define (cost c)
  (if(equal? c 'M)1 0))
(define (Query l x)
  (if(null? l)#f
     (if(equal? x (car l))#t
        (Query(cdr l) x))))
(define (query x)(Query s x ))
  
(define (solve r c k)
  (set! M (Mread r c))
  (set! s (list(list 1 1 k)))
  (set! d 1)
  (define ans -1)
  (define (work ss)
    (define S '())
    (define (ok x y k)
      (cond((< x 1)#f)
           ((> x r)#f)
           ((< y 1)#f)
           ((> y c)#f)
           ((and (= k 0)(equal? (A x y M) 'M))#f)
           ((equal? (A x y M) 'W)#f)
           ((query(list x y (- k (cost (A x y M)))))#f)
           (else (if(and (= x r)(= y c))(begin(set! ans d)#t)#t))))
    (define (ins x y k)
      (if(ok x y k)(set! S (cons (list x y (- k (cost (A x y M)))) S))(void)))
    (define (Ins x y k)
      (ins (+ x 1) y k)
      (ins (- x 1) y k)
      (ins x (+ y 1) k)
      (ins x (- y 1) k))
    (define (check l)
      (if(null? l)(void)
         (begin (Ins (caar l)(cadar l)(caddar l))(check(cdr l)))))
    (if(null? ss)(void)
       (begin(check ss)
       (set! s (append s S))
       (if(= ans -1)(begin(set! d (+ d 1))(work S))
          (void)))))
  (work s)
  (if(= ans -1)(displayln 'inf)
     (displayln ans)))
(define (myloop n)
  (if(= n 0)(void)
    (begin (solve (read)(read)(read))(myloop (- n 1)))))


(myloop(read))

by 1700013005
#lang racket
(define (Case)
  (let ((N (read)) (M (read)) (K (read)))
    (define (load i)
      (define (load-line i) (if (= i M) '() (cons (read) (load-line (+ i 1)))))
      (if (= i N) '() (cons (load-line 0) (load (+ i 1)))))
    (let ((B (load 0)))
      (define (elem x y) (list-ref (list-ref B (- x 1)) (- y 1)))
      (define (H x y k) (+ k (* 100 (+ y (* 100 x)))))
      (define (SH t) (if (null? t) '() (let ((t (car t)))(list (H (car t) (cadr t) (caddr t))))))
      (define (GS t dx dy S)
        (let ((x (car t)) (y (cadr t)) (k (caddr t)) (s (cadddr t)))
          (let ((tx (+ x dx)) (ty (+ y dy)))
            (if (and (<= 1 tx) (<= tx N) (<= 1 ty) (<= ty M))
                (let ((e (elem tx ty)))
                  (let ((tk (if (and (eq? e 'M) (> k 0)) (- k 1) k)))
                    (if (and (not (memq (H tx ty tk) S)) (or (eq? e 'B) (and (eq? e 'M) (> k 0))))
                        (list (list tx ty tk (+ s 1)))
                        '())))
                '()))))
      (define (bfs q S)
        (if (null? q)
            "inf"
            (let ((t (car q)) (rq (cdr q)))
              (let ((x (car t)) (y (cadr t)) (k (caddr t)) (s (cadddr t)))
                (if (and (= x N) (= y M))
                    s
                    (let ((s1 (GS t 0 1 S)) (s2 (GS t 0 -1 S)) (s3 (GS t 1 0 S)) (s4 (GS t -1 0 S)))
                      (let ((Ls (append s1 s2 s3 s4)) (Lh (append (SH s1) (SH s2) (SH s3) (SH s4))))
                        (bfs (append rq Ls) (append S Lh)))))))))
      (let ((init-state (list 1 1 K 0))) (bfs (list init-state) (SH (list init-state)))))))

(define (Test n)
  (cond ((> n 0) (begin (displayln (Case)) (Test (- n 1))))))

(Test (read))

by 1800012803(Lavintan)
#lang racket
(define-syntax-rule (defmacro x ...) (define-syntax-rule x ...))
(defmacro (setf x ...) (set! x ...))
(defmacro (asetf x ...) (vector-set! x ...))
(defmacro (aref x ...) (vector-ref x ...))
(defmacro (1+ x) (add1 x))
(defmacro (1- x) (sub1 x))
(defmacro (div x ...) (quotient x ...))
(defmacro (rem x ...) (remainder x ...))
(require data/queue)
(define (read-map n m)
  (define ret (make-vector n))
  (define (rm i j)
    (when (< i n)
      (when (= j 0)
        (asetf ret i (make-vector m)))
      (let ([c (read)])
        (cond
          ([eq? c 'B] (asetf (aref ret i) j 1))
          ([eq? c 'M] (asetf (aref ret i) j 2))
          ([eq? c 'W] (asetf (aref ret i) j 0))))
      (if (< j (1- m))
          (rm i (1+ j))
          (rm (1+ i) 0))))
  (rm 0 0)
  ret)
(define (solve n m k)
  (define q (make-queue))
  (define s (mutable-set))
  (define puz (read-map n m))
  (define (puzzle x y)
    (aref (aref puz x) y))
  (define (check i j c d)
    (when (and (>= i 0) (< i n) (>= j 0) (< j m))
      (let ([g (puzzle i j)])
        (when (> g 0)
          (when (= g 2)
            (set! c (1- c)))
          (when (and (>= c 0) (not (set-member? s (list i j c))))
            (set-add! s (list i j c))
            (enqueue! q (list i j c (1+ d))))))))
  (enqueue! q `(0 0 ,k 0))
  (define (run)
    (if (not (queue-empty? q))
      (let ([cur (dequeue! q)])
        (let ([i (first cur)] [j (second cur)] [c (third cur)] [d (fourth cur)])
          (if (and (= i (1- n)) (= j (1- m)))
              d
              (begin
                (check i (1+ j) c d)
                (check (1+ i) j c d)
                (check (1- i) j c d)
                (check i (1- j) c d)
                (run)))))
      'inf))
  (run))
(define (main n)
  (when (> n 0)
    (displayln (solve (read) (read) (read)))
    (main (1- n))))
(main (read))

by 1700013005
#lang racket
(define (Case)
  (let ((N (read)) (M (read)) (K (read)))
    (define (load i)
      (define (load-line i) (if (= i M) '() (cons (read) (load-line (+ i 1)))))
      (if (= i N) '() (cons (load-line 0) (load (+ i 1)))))
    (let ((B (load 0)))
      (define (elem x y) (list-ref (list-ref B (- x 1)) (- y 1)))
      (define (H x y k) (+ k (* 100 (+ y (* 100 x)))))
      (define (SH t)
        (if (null? t)
            '()
            (let ((t (car t)))(list (H (car t) (cadr t) (caddr t))))))
      (define (GS t dx dy S)
        (let ((x (car t)) (y (cadr t)) (k (caddr t)) (s (cadddr t)))
          (let ((tx (+ x dx)) (ty (+ y dy)))
            (if (and (<= 1 tx) (<= tx N) (<= 1 ty) (<= ty M))
                (let ((e (elem tx ty)))
                  (let ((tk (if (and (eq? e 'M) (> k 0)) (- k 1) k)))
                    (if (and (not (memq (H tx ty tk) S)) (or (eq? e 'B) (and (eq? e 'M) (> k 0))))
                        (list (list tx ty tk (+ s 1)))
                        '())))
                '()))))
      (define (bfs q S)
        ;(displayln (list 'q q))
        ;(displayln (list 'S S))
        (if (null? q)
            "inf"
            (let ((t (car q)) (rq (cdr q)))
              (let ((x (car t)) (y (cadr t)) (k (caddr t)) (s (cadddr t)))
                (if (and (= x N) (= y M))
                    s
                    (let ((s1 (GS t 0 1 S)) (s2 (GS t 0 -1 S)) (s3 (GS t 1 0 S)) (s4 (GS t -1 0 S)))
                      ;(displayln (list s1 s2 s3 s4))
                      (let ((Ls (append s1 s2 s3 s4)) (Lh (append (SH s1) (SH s2) (SH s3) (SH s4))))
                        (bfs (append rq Ls) (append S Lh)))))))))
      (let ((init-state (list 1 1 K 0)))
        (bfs (list init-state) (SH (list init-state)))))))

(define (Test n)
  (cond ((> n 0) (begin (displayln (Case)) (Test (- n 1))))))

(Test (read))

****4: 抓住那头牛
by 1700013005
#lang racket
(define n (read))
(define k (read))
(define g (* k 2))
(define S (make-vector g #f))
(define q (make-vector g))
(define h -1)
(define r 1)
(define (GS x s S)
  (if (or (< x 0) (>= x g) (vector-ref S x))
      -1
      (begin (vector-set! S x #t) (vector-set! q r (list x (+ s 1))) (set! r (+ r 1)) x)))
(define (bfs)
  (set! h (+ h 1))
  (let ((t (vector-ref q h)))
    (let ((x (car t)) (s (cadr t)))
      (if (= x k) s (begin (GS (- x 1) s S) (GS (+ x 1) s S) (GS (* x 2) s S) (bfs))))))
(displayln (if (>= n k) (- n k) (begin (vector-set! S n #t) (vector-set! q 0 (list n 0)) (bfs))))

by 1800012901
#lang racket
(define dis (make-vector 200000 -1))
(define q (make-vector 300000 0))
(define f 1)
(define w 0)
(define (bfs s t)
	(vector-set! q 1 s)
	(vector-set! dis s 0)
	(define (gao x d)
		(if (and (>= x 0) (< x 200000) (= (vector-ref dis x) -1))
			(begin
				(vector-set! dis x d)
				(set! f (+ f 1))
				(vector-set! q f x)
			)
			(void)
		)
	)
	(define (loop)
		(if (<= f w)
			(void)
			(let ((x (vector-ref q (+ w 1))))
				(set! w (+ w 1))
				(let ((d (vector-ref dis x)))
					(if (= x t)
						d
						(begin
							(gao (+ x 1) (+ d 1))
							(gao (- x 1) (+ d 1))
							(gao (* x 2) (+ d 1))
							(loop)
						)
					)
				)
			)
		)
	)
	(displayln (loop))
)
(bfs (read) (read))

by 1700013005
#lang racket
(define n (read))
(define k (read))
(define g (max n (* k 2)))
(define G (+ 1 g))
(define S (make-vector G #f))
(vector-set! S n #t)
(define q (make-vector G))
(vector-set! q 0 (list n 0))
(define h -1)
(define r 1)
(define (GS x s S)
  (if (or (< x 0) (>= x g) (vector-ref S x))
      -1
      (begin (vector-set! S x #t) (vector-set! q r (list x (+ s 1))) (set! r (+ r 1)) x)))
(define (bfs)
  (set! h (+ h 1))
  (let ((t (vector-ref q h)))
    (let ((x (car t)) (s (cadr t)))
      (let ((s1 (GS (- x 1) s S)) (s2 (GS (+ x 1) s S)) (s3 (GS (* x 2) s S)))
        (cond
          ((= s1 k) (+ s 1))
          ((= s2 k) (+ s 1))
          ((= s3 k) (+ s 1))
          (else (bfs)))))))
(displayln (if (= n k) 0 (bfs)))

by 1800012803(Lavintan)
#lang racket

(define queue (make-vector 600000 0))
(define vis (make-vector 600000 0))
(define dis (make-vector 600000 0))
(define (push x d)
    (if(and (>= x 0) (<= x 500000) (eq? 0 (vector-ref vis x)))
       (begin
         (vector-set! vis x 1)
         (vector-set! dis x (+ d 1))
         (set! r (+ r 1))
         (vector-set! queue r x)) (void)))
(define (get w)
    (define curdis (vector-ref dis w))
    (push (- w 1) curdis)
    (push (+ w 1) curdis)
    (push (* w 2) curdis)
    (void))
(define r 1)
(define (bfs st)
  (define (solve l)
    (if(> l r)(void)
       (begin
         (get (vector-ref queue l))
         (solve (+ 1 l)))))
  (vector-set! queue 1 st)
  (vector-set! vis st 1)
  (vector-set! dis st 0)
  (solve 1))


(define k1 (read))
(define ter (read))
(bfs k1)
(displayln (vector-ref dis ter))

