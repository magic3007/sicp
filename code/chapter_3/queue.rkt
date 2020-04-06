#lang racket

(require scheme/mpair)
(define car mcar)
(define cdr mcdr)
(define cadr (lambda (lst) (mcar (mcdr lst))))
(define list mlist)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
(define cons mcons)

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


(define q (make-que))
(insert-que! q 'a)
(insert-que! q 'b)
(delete-que! q)
(insert-que! q 'c)
(insert-que! q 'd)
(delete-que! q)


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