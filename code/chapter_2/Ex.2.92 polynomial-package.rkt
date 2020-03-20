#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现,不须搞明白也能完成本题
(require scheme/mpair)
(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      (void))    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define conversion-table (make-table))
(define get-coercion (conversion-table 'lookup-proc))
(define put-coercion (conversion-table 'insert-proc!))
;以上是put和get的实现,不须搞明白也能完成本题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;------------- integer package
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))    
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) ((get 'make 'rational )  x y)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (void))

(define (make-integer n)
  ((get 'make 'integer) n))


;--------general functions
  
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (install-polynomial-package)
  (put 'make 'polynomial
       (lambda (var terms) `(polynomial ,var . ,terms)))
  (put 'make 'polynomial-term (lambda x x))
  (define (tag x) (attach-tag 'polnomial x))
  (define (add-terms t1 t2)
    (match* (t1 t2)
      [(t1 '()) t1]
      [('() t2) t2]
      [(`((,order1 ,coeff1) . ,rest1) `((,order2 ,coeff2) . ,rest2))
       (match (- order1 order2)
             [(? zero?) `((,order1 ,(add coeff1 coeff2)) . ,(add-terms rest1 rest2))]
             [(? positive?) `((,order1 ,coeff1) . ,(add-terms rest1 t2))]
             [(? negative?) (add-terms t2 t1)])]))
  (define (combine t)
    (match t
      [`((,order ,coeff1) (,order ,coeff2) . ,rest)
       (combine `((,order ,(add coeff1 coeff2)) . ,rest))]
      [`(,x . ,rest)
       `(,x . ,(combine rest))]
      ['() '()]))
  (define (mul-terms t1 t2)
    (let* ([result
            (append-map (lambda (term1)
                          (map (lambda (term2)
                                 (match* (term1 term2)
                                   [(`(,order1 ,coeff1) `(,order2 ,coeff2))
                                    `(,(+ order1 order2) ,(mul coeff1 coeff2))]))
                               t2))
                        t1)])
      (combine (sort result > #:key car))))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2)
         `(polynomial ,(first p1) . ,(add-terms (rest p1) (rest p2)))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2)
         `(polynomial ,(first p1) . ,(mul-terms (rest p1) (rest p2)))))
  )

(define (apply-generic op e1 e2)
  (define (priority e)
    (match e
      [`(integer . ,_) 0]
      [`(polynomial ,var . ,_) (case var ['e 1] ['d 2] ['c 3] ['b 4] ['a 5])]))
   (match* (e1 e2)
     [(`(,type1 . ,content1) `(,type2 . ,content2))
       (match (- (priority e1) (priority e2))
           [(? zero?) ((get op `(,type1 ,type2)) content1 content2)]
           [(? positive?) (apply-generic op e1 (make-poly (second e1) (list (make-term 0 e2))))]
           [(? negative?) (apply-generic op e2 e1)])]))

(define (display-poly p)
  (define (display-terms t)
    (match t
      ['() '()]
      [`((,order ,coeff) . ,rest)
       `((,order ,(display coeff)) . ,(display-terms rest))]))
  (define (display p)
    (match p
      [`(integer . ,x) x]
      [`(polynomial ,var . ,rest)
       (cons var (display-terms rest))]))
  (println (display p)))

(define (build-poly e)
  (define (build-terms t)
    (match t
      ['() '()]
      [`((,order ,coeff) . ,rest)
       (cons (make-term order (build-poly coeff)) (build-terms rest))]))
  (match e
    [(? number?) (make-integer e)]
    [`(,var . ,rest) (make-poly var (build-terms rest))]))
  
(install-integer-package)
(install-polynomial-package)
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))


(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))
(define (make-term order coeff) 
  ((get 'make 'polynomial-term) order coeff))

(displayln "******1")
(define e1 (make-poly 'a (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3a+2
(define e2 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4a^2 + 3a
(displayln e1)
(displayln e2)
(displayln (add e1 e2))
(displayln (mul e1 e2))

(displayln "******2")

(define c1 (make-poly 'b (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3b+2
(define c2 (make-poly 'b (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4b^2 + 3b

(define e3 (make-poly 'a (list (list 1 c1) (list 0 (make-integer 2))))) 
(define e4 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 c2)))) 

(displayln (add e3 e4))

(displayln "******")
(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (let ((op (car a))
              (e1 (cadr a))
              (e2 (caddr a)))
          (if (eq? op '+)
              (display-poly (add (build-poly e1) (build-poly e2)))
              (display-poly (mul (build-poly e1) (build-poly e2))))
          (myloop)))))
              
(myloop)