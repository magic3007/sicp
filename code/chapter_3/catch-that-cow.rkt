#lang racket

(require scheme/mpair)
(define cdr mcdr)
(define car mcar)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
(define cons mcons)
(define assoc massoc)

;;;;;;;;;;;all queue thing
(define (front-ptr que) (car que))
(define (rear-ptr que) (cdr que))

(define (set-front-ptr! que item) (set-car! que item))
(define (set-rear-ptr! que item) (set-cdr! que item))
(define (empty-que? que) (null? (front-ptr que)))

(define (make-que) (cons '() '()))

(define (front-que que)
  (if (empty-que? que) 
      (error "emtpy que" que)
      (car (front-ptr que))))

(define (rear-que que)
  (if (empty-que? que) 
      (error "emtpy que" que)
      (car (rear-ptr que))))

(define (insert-que! que item)
  (let ((new-pair (cons item '())))
    (cond ((empty-que? que)
           (set-front-ptr! que new-pair)
           (set-rear-ptr! que new-pair)
           que)
          (else
           (set-cdr! (rear-ptr que) new-pair)
           (set-rear-ptr! que new-pair)
           que))))
                          

(define (delete-que! que)
  (cond ((empty-que? que) (error "delete empty que" que))
         (else (set-front-ptr! que (cdr (front-ptr que)))
               que)))

(define (print-que que)
  (define (iter ptr)
    (if (null? ptr)
        (display "")
        (begin (display (car ptr)) (iter (cdr ptr)))))
  (iter (front-ptr que)))

;;end of queue thing

(define max_n 100000)

(define updateFuncs (list
                          (lambda (x) (add1 x))
                          (lambda (x) (sub1 x))
                          (lambda (x) (+ x x))))


(define (valid? x) (and (0 . <= . x) (x . <= . max_n)))

(define (solve n k)
  (let/cc return
    (define (answer? lst)
      (if (= k (first lst)) (return (second lst))(void)))
    (let* ([que (make-que)]
           [visited (make-vector (add1 max_n) 0)])
      (define (try-insert lst)
        (if (= (vector-ref visited (first lst)) 0)
            (begin (insert-que! que lst)
             (vector-set! visited (first lst) 1))
            (void)))
      (try-insert `(,n 0))
      (let loop ([x #f])
        (if (empty-que? que) (return #f) (void))
        (set! x (front-que que))
        (delete-que! que)
        (answer? x)
        (match x
          [`(,a ,b)
            (for-each (lambda (x) (try-insert `(,x ,(add1 b))))
                    (filter valid?
                            (map (lambda (proc) (proc a))
                                   updateFuncs)))]
          [_ (error "solve")])
        (loop #f)))))

(displayln (solve (read) (read)))



;;

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

