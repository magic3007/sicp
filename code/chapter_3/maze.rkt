#lang racket

(require scheme/mpair)

; queue begin
(define-struct que ([head #:mutable] [tail #:mutable]))

(define (empty-que? que) (null? (que-head que)))
(define (push-que! que item)
  (let ([new-pair (mcons item `())])
    (if (empty-que? que)
        (set-que-head! que new-pair)
        (set-mcdr! (que-tail que) new-pair))
    (set-que-tail! que new-pair)))
(define (pop-que! que)
  (begin0 (mcar (que-head que))
          (set-que-head! que (mcdr (que-head que)))))
; queue end

(define (make-2dvector r c init)
  (build-vector r (lambda (x) (make-vector c init))))

(define (2dvector-ref vec a b)
  (vector-ref (vector-ref vec a) b))

(define (2dvector-set! vec a b v)
  (vector-set! (vector-ref vec a) b v))


(define (read-2d-vector r c)
  (let ([vec (make-2dvector r c #f)])
    (for ([i r] #:when #t
          [j c])
      (2dvector-set! vec i j (read)))
    vec))

(define inf 100000000000)

(define (solve)
  (let/cc return
    (let*-values ([(r c k) (values (read) (read) (read))]
                  [(maze) (values (read-2d-vector r c))]
                  [(q) (values (make-que '() '()))]
                  [(visited) (values (make-hash))])

      (define (answer? lst)
        (match lst
          [`(,dist ,x ,y ,k)
           (if (and (= x (sub1 r)) (= y (sub1 c)))
               (return dist)
               #f)]))
      
      (define update-funcs (list
                            (lambda (x y k) `(,(add1 x) ,y ,k))
                            (lambda (x y k) `(,(sub1 x) ,y ,k))
                            (lambda (x y k) `(,x ,(add1 y) ,k))
                            (lambda (x y k) `(,x ,(sub1 y) ,k))))

      (define (valid? lst)
        (match lst
          [`(,x ,y ,k) (and (0 . <= . x) (x . < . r)
                            (0 . <= . y) (y . < . c)
                            (not (eq? (2dvector-ref maze x y) `W))
                            (or (k . >= . 1) (not (eq? (2dvector-ref maze x y) `M))))]
          [_ (error "valid")]))

      (define (update-k lst)
        (match lst
          [`(,x ,y ,k)
           (if (eq? (2dvector-ref maze x y) `M)
               `(,x ,y ,(sub1 k))
               lst)]
          [_ (error "update-k")]))

      (define(visited? lst)
        (hash-ref visited lst #f))

      (define (loop x)
        (when (empty-que? q) (return 'inf))
        (set! x (pop-que! q))
        (answer? x)
        (match x
          [`(,dist ,x ,y ,k)
           (for-each (lambda (lst)
                       (hash-set! visited lst (add1 dist))
                       (push-que! q (cons (add1 dist) lst)))
                     (filter-not visited?
                             (map update-k
                                  (filter valid?
                                          (map (lambda (proc) (proc x y k)) update-funcs)))))])
        (loop #f))
      (push-que! q (cons 0 `(,0 ,0 ,k)))
      (loop #f))))
                  
                    
                                             
(define (readcases)
  (let loop ([m (read)])
    (unless (eq? m 0)
        (begin (displayln (solve)) (loop (sub1 m))))))

(readcases)


;
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