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
