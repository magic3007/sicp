#lang racket

(define (make-barber n)
  (let* ([items (build-list n values)])
    (define (foo op . args)
      (match op
        ['get
         (if (null? items)
             -1
             (let ([rv (first items)]
                   [rest (cdr items)])
               (set! items rest)
               rv))]
        ['add
         (begin
           (set! items (append args items))
           (set! items (sort items <)))]))
  foo))


(define (work barber lst)
  (if (null? lst)
      (void)
      (let* ([sorted (sort lst #:key car <)]
             [x (first sorted)]
             [rest (cdr sorted)])
        (match x
          [`(,a ,b)
           (let ([rv (barber 'get)])
             (if (eq? rv -1)
                 (begin
                   (displayln "Failed")
                   (work barber rest))
                 (begin
                   (displayln rv)
                   (work barber `((,b ,rv end) . ,rest)))))]
          [`(,a ,b ,_)
           (begin
             (barber 'add b)
             (work barber rest))]))))

(define (solve n m)
  (define (read-cust acc m lst)
        (if (eq? m 0)
            lst
            (let-values ([(a b) (values (read) (read))])
              (read-cust (+ acc a) (sub1 m) (cons (list (+ acc a) (+ acc a b)) lst)))))
    (let* ([barber (make-barber n)]
           [lst (reverse  (read-cust 0 m '()))])
      (work barber lst)))

(define (readcases)
  (let*-values ([(n m) (values (read) (read))])
    (if (eq? n eof)
        (void)
        (begin (solve n m) (readcases)))))

(readcases)