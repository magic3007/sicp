****1: SICP 课本练习2.92
by 1700012751
#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现，不须搞明白也能完成本题
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
;以上是put和get的实现，不须搞明白也能完成本题
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
  (displayln (display p)))

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

by 1800012907
#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现，不须搞明白也能完成本题
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
;以上是put和get的实现，不须搞明白也能完成本题
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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (apply proc (map contents args)))))

(define (install-polynomial-package)
  (define (symbol->integer x)
    (char->integer (car (string->list (symbol->string x)))))
  (define (less? x y)
    (< (symbol->integer (car x)) (symbol->integer (car y))))
  (define (tag x) (attach-tag 'polynomial x))
  (define (make-poly var lst) (cons var lst))
  (define (add-terms l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          (else
           (let ((t1 (car l1)) (t2 (car l2)))
             (cond ((> (car t1) (car t2)) (cons t1 (add-terms (cdr l1) l2)))
                   ((< (car t1) (car t2)) (cons t2 (add-terms l1 (cdr l2))))
                   (else
                    (cons (list (car t1)
                                (add (cadr t1) (cadr t2)))
                     (add-terms (cdr l1) (cdr l2)))))))))
  (define (add-poly x y)
    (if (eq? (car x) (car y))
        (make-poly (car x) (add-terms (cdr x) (cdr y)))
        (if (less? x y)
            (make-poly (car x) (add-terms (cdr x) (list (list 0 (tag y)))))
            (make-poly (car y) (add-terms (cdr y) (list (list 0 (tag x))))))))
  (define (add-poly-integer x y)
    (make-poly (car x) (add-terms (cdr x) (list (list 0 (attach-tag 'integer y))))))
  (define (add-integer-poly x y)
    (add-poly-integer y x))
  (define (mul-terms-by-all-terms t L)
    (if (null? L)
        '()
        (add-terms
         (list (list (+ (car t) (car (car L)))
                     (mul (cadr t) (cadr (car L)))))
         (mul-terms-by-all-terms t (cdr L)))))
  (define (mul-terms l1 l2)
    (if (null? l1)
        '()
        (add-terms (mul-terms-by-all-terms (car l1) l2)
                   (mul-terms (cdr l1) l2))))
  (define (mul-poly x y)
    (if (eq? (car x) (car y))
        (make-poly (car x) (mul-terms (cdr x) (cdr y)))
        (if (less? x y)
            (make-poly (car x) (mul-terms (cdr x) (list (list 0 (tag y)))))
            (make-poly (car y) (mul-terms (cdr y) (list (list 0 (tag x))))))))
  (define (mul-poly-integer x y)
    (make-poly (car x) (mul-terms (cdr x) (list (list 0 (attach-tag 'integer y))))))
  (define (mul-integer-poly x y)
    (mul-poly-integer y x))
  (put 'make 'polynomial
       (lambda (x y) (tag (make-poly x y))))
  (put 'add '(polynomial polynomial)
       (lambda (x y) (tag (add-poly x y))))
  (put 'add '(polynomial integer)
       (lambda (x y) (tag (add-poly-integer x y))))
  (put 'add '(integer polynomial)
       (lambda (x y) (tag (add-integer-poly x y))))
  (put 'mul '(polynomial polynomial)
       (lambda (x y) (tag (mul-poly x y))))
  (put 'mul '(polynomial integer)
       (lambda (x y) (tag (mul-poly-integer x y))))
  (put 'mul '(integer polynomial)
       (lambda (x y) (tag (mul-integer-poly x y)))))

(define (display-poly x)
  (define (concise-terms L)
    (if (null? L)
        '()
        (cons (list (car (car L)) (concise (cadr (car L)))) (concise-terms (cdr L)))))
  (define (concise x)
    (if (eq? (car x) 'integer)
        (cdr x)
        (cons (cadr x) (concise-terms (cddr x)))))
  (displayln (concise x)))

(define (build-poly x)
  (define (formal-terms L)
    (if (null? L)
        '()
        (cons (list (car (car L)) (formal (cadr (car L)))) (formal-terms (cdr L)))))
  (define (formal x)
    (if (number? x)
        (cons 'integer x)
        (cons 'polynomial (cons (car x) (formal-terms (cdr x))))))
  (formal x))

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

by 1900012959(lny)
#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现，不须搞明白也能完成本题
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
;以上是put和get的实现，不须搞明白也能完成本题
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
  (define (tag x) (attach-tag 'polynomial x))
  (define (lsTOterm ls)
    ;(display "lsToterm ")(displayln ls)
    (cons (car ls) (map
                    (lambda (x)
                      (if (list? x) (if (eq? (car x) 'polynomial) (make-poly (cadr x) (cddr x))
                                        (make-poly (car x) (cdr x)))
                                    (if (number? x) (make-integer x)
                                        x)))
                    (cdr ls))
          )
    )
  (define (maketerms ls)
    ;(display "maketerms ")(displayln ls)
    (if (null? ls) '()
        (cons (lsTOterm (car ls)) (maketerms (cdr ls)))
        )
    )
  (define (addterms x y)
    ;(display "addterm ")(display x)(displayln y)
    (if (null? y) x
        (if (null? x) y
            (if (> (caar x) (caar y)) (cons (car x) (addterms (cdr x) y))
                (if (< (caar x ) (caar y)) (cons (car y) (addterms x (cdr y)))
                    (cons (list (caar x) (add (cadar x) (cadar y))) (addterms (cdr x) (cdr y)))
                    )
                )
            )))
  (put 'add '(polynomial polynomial)
       (lambda (x y)
         (tag (if (eq? (car x) (car y))
             (cons (car x) (addterms (cdr x) (cdr y)))
             (if (string<? (symbol->string (car x)) (symbol->string (car y)))
                      (cons (car x) (addterms (list (list 0 (tag y))) (cdr x)))
                      (cons (car y) (addterms (list (list 0 (tag x))) (cdr y)))
                      )
                  )
              )
             ;(cons (car x) (addterms (cdr x) (list (list 0 y))))
             ;))
         )
       )
  (define (multerms x y)
    (if (null? x) '()
        (addterms
         (map (lambda (t) ;(displayln (mul (cadar x) (cadr t)))
                (list (+ (caar x) (car t)) (mul (cadar x) (cadr t))))
              y)
         (multerms (cdr x) y))
        )
    )
  (put 'mul '(polynomial polynomial)
       (lambda (x y)
         (tag (if (eq? (car x) (car y))
                  (cons (car x) (multerms (cdr x) (cdr y)))
                  (if (string<? (symbol->string (car x)) (symbol->string (car y)))
                      (cons (car x) (multerms (list (list 0 (tag y))) (cdr x)))
                      (cons (car y) (multerms (list (list 0 (tag x))) (cdr y)))
                      )
                  )
              )
         )
       )
  (put 'add (list 'integer 'polynomial)
       (lambda (x y)
         (tag (cons (car y) (addterms (list (list 0 (make-integer x))) (cdr y))))
         )
       )
  (put 'add (list 'polynomial 'integer)
       (lambda (x y)
         (tag (cons (car x) (addterms (list (list 0 (make-integer y))) (cdr x))))
         )
       )
  (put 'mul (list 'integer 'polynomial)
       (lambda (y x)
         (tag (cons (car x) (multerms (list (list 0 (make-integer y))) (cdr x))))
         )
       )
  (put 'mul (list 'polynomial 'integer)
       (lambda (x y)
         (tag (cons (car x) (multerms (list (list 0 (make-integer y))) (cdr x))))
         )
       )
  ;(put 'div '(polynolmial polynolmial)
      ; (lambda (x y) ((get 'make 'rational )  x y)))
  (put 'make 'polynomial
       (lambda (x y) (tag (cons x (maketerms y))))
       )
  (void))
(define (apply-generic op x y)
  ;(display "apply ")(display op)(display x)
  ;(displayln y)
  ((get op (list (type-tag x) (type-tag y)))
   (cdr x) (cdr y))
  )
(define (display-poly x)
  (define (f1 x)
    (if (list? x)
        (cons (cadr x) (map (lambda (t) (list (car t) (f1 (cadr t)))) (cddr x)))
        (if (cons? x) (cdr x)
            #f)
        )
    )
  (displayln (f1 x))
  )
(define (build-poly x)
  (make-poly (car x) (cdr x))
  )
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

by 1800012803(Lavintan)
#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现，不须搞明白也能完成本题
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
;以上是put和get的实现，不须搞明白也能完成本题
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
(define tmp 0)
(define (add-arbitrary x y)
  (begin
    (if(eq? (car x) 'integer)
       (begin
         (set! tmp x)
         (set! x y)
         (set! y tmp))
       (void))
    ((get 'add (list (car x) (car y))) (cdr x) (cdr y))))

(define (mul-arbitrary x y)
  (begin
    (if(eq? (car x) 'integer)
       (begin
         (set! tmp x)
         (set! x y)
         (set! y tmp))(void))
    ((get 'mul (list (car x) (car y))) (cdr x) (cdr y))))

(define (set-tag x)
  (if(real? x) (cons 'integer x) (cons 'polynomial x)))

(define (install-polynomial-package)
  (define (val w)
    (cond ((eq? w 'a) 0)
          ((eq? w 'b) 1)
          ((eq? w 'c) 2)
          ((eq? w 'd) 3)
          ((eq? w 'e) 4)))

  (define (add-number p1 w)
    (cond
      ((null? p1) (list (list 0 (set-tag w))))
      ((= (car (car p1)) 0)
       (list (list 0 (add-arbitrary (cadr (car p1)) (set-tag w)))))
      (else (cons (car p1) (add-number (cdr p1) w)))))

  (define (add-polynomial p1 p2)
    (cond
      ((null? p1) p2)
      ((null? p2) p1)
      ((< (car (car p1)) (car (car p2))) (add-polynomial p2 p1))
      ((= (car (car p1)) (car (car p2)))
       (cons (list (car (car p1))
                   (add-arbitrary (cadr (car p1))
                                  (cadr (car p2))))
             (add-polynomial (cdr p1) (cdr p2))))
      (else (cons (car p1) (add-polynomial (cdr p1) p2)))))

  (define (mul-number p1 w)
    (cond
      ((null? p1) '())
      (else (append (list (list (car (car p1))
                                (mul-arbitrary (cadr (car p1))
                                               (set-tag w))))
                    (mul-number (cdr p1) w)))))

  (define (mul-polynomial p1 p2)
    (cond
      ((null? p1) '())
      (else (add-polynomial (map (lambda (x)
                                   (list (+ (car (car p1)) (car x))
                                         (mul-arbitrary (cadr (car p1))
                                                        (cadr x))))
                                 p2)
                            (mul-polynomial (cdr p1) p2)))))

  (put 'add '(polynomial integer)
       (lambda (x y) (cons 'polynomial(cons (car x) (add-number (cdr x) y)))))

  (put 'add '(polynomial polynomial)
       (lambda (x y)
         (cond
           ((eq? (car x) (car y)) (cons 'polynomial (cons (car x) (add-polynomial (cdr x) (cdr y)))))
           ((<   (val (car x)) (val (car y))) (cons 'polynomial (cons (car x) (add-number (cdr x) y))))
           ((>   (val (car x)) (val (car y))) (cons 'polynomial (cons (car y) (add-number (cdr y) x)))))))

  (put 'mul '(polynomial integer)
       (lambda (x y) (cons 'polynomial (cons (car x) (mul-number (cdr x) y)))))

  (put 'mul '(polynomial polynomial)
       (lambda (x y)
         (cond
           ((eq? (car x) (car y)) (cons 'polynomial (cons (car x) (mul-polynomial (cdr x) (cdr y)))))
           ((< (val (car x)) (val (car y))) (cons 'polynomial (cons (car x) (mul-number (cdr x) y))))
           ((> (val (car x)) (val (car y))) (cons 'polynomial (cons (car y) (mul-number (cdr y) x)))))))

  (put 'make 'polynomial (lambda (x y) (cons 'polynomial (cons x y))))

  (put 'make 'polynomial-term (lambda (x y) (cons 'polynomial (cons x y)))))


(define (build-term x)
  (list (car x) (build-poly (cadr x))))

(define (gt x)
  (if(null? x) '()
    (append (list (build-term (car x))) (gt (cdr x)))))

(define (build-poly x)
  (if(real? x)
     (make-integer x)
     (cons 'polynomial (cons (car x) (gt (cdr x))))))

(define (pp w)
  (if(equal? (car w) 'integer) (cdr w)
     (zheng (cdr w))))

(define (work x)
  (list (car x) (pp (cadr x))))

(define (kzheng x)
  (if(null? x) '()
     (append (list (work (car x))) (kzheng (cdr x)))))
  
(define (zheng x)
  (cons (car x) (kzheng (cdr x))))

(define (display-poly x)
  (displayln (zheng (cdr x))))

(define (apply-generic op k1 k2)
  ((get op (list (car k1) (car k2))) (cdr k1) (cdr k2)))
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

****2: SICP课本练习2.84
by 1900012959(lny)
#lang racket
(define (square x) (* x x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现，不须搞明白也能完成本题
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
;以上是put和get的实现，不须搞明白也能完成本题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;---------- about tags:
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

;--------- rectangular compex package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (void))

;----------- polar complex package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (void))



;----------- rational package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'rational 'numer
       (lambda (x) (numer (contents x)))) ; x is tagged rational
  (put 'rational 'denom
       (lambda (x) (denom (contents x)))) ; x is tagged rational

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (void))
(define (make-rational n d)
  ((get 'make 'rational) n d))




;---------- high level complex package
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
;  (define (mul-complex z1 z2)
;    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
;                       (+ (angle z1) (angle z2))))
;  (define (div-complex z1 z2)
;    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
;                       (- (angle z1) (angle z2))))
 (define (mul-complex z1 z2)
   (let ((a (real-part z1))
         (b (imag-part z1))
         (c (real-part z2))
         (d (imag-part z2)))
     (make-from-real-imag (- (* a c) (* b d)) (+ (* a d) (* b c)))))

 (define (div-complex z1 z2)
   (let ((a (real-part z1))
         (b (imag-part z1))
         (c (real-part z2))
         (d (imag-part z2)))
     (let ((denom (+ (square c) (square d))))
       (make-from-real-imag (/ (+ (* a c) (* b d)) denom)
                            (/ (- (* b c) (* a d)) denom)))))

  ;(a+bi)/(c+di) =(a+bi)*(c-di)/(c+di)*(c-di)=(ac-adi+bci+bd)/(c*c+d*d)=(ac+bd)/(c^2+d^2)+〔(bc-ad)/(c^2+d^2)〕i  
  
  
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (void))

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

;---------------- real package
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))    
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag (+ x 0.0))))
  (void))

(define (make-real x)
  ((get 'make 'real) x))


;----------- general functions
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))






(define (install-raise-package) ;install raise functions 
  (define (raise-integer n)
    (make-rational (contents n) 1))
(define (raise-rational n)
    (make-real (/ (car (contents n)) (cdr (contents n)))))
  (define (raise-real n)
    (make-complex-from-mag-ang (contents n) 0))
  (put 'raise 'integer raise-integer)
  (put 'raise 'rational raise-rational)
  (put 'raise 'real raise-real)
  )
(define (coercion-tower x y)
  (define (f1 a b ls)
    (if (eq? (type-tag a) (type-tag b)) (list a b)
        (if (eq? (car ls) (type-tag a)) (f1 ((get 'raise (car ls)) a) b (cdr ls))
            (if (eq? (car ls) (type-tag b)) (f1 a ((get 'raise (car ls)) b) (cdr ls))
                (f1 a b (cdr ls))
                )
            )))
  (f1 x y '(integer rational real))
  )
(define (apply-generic op . args)
  (if (null? (cdr args))
      ((get op (map type-tag args)) (contents (car args)))
      (let [(coer (apply coercion-tower args))]
        (apply (get op (map type-tag coer)) (map contents coer))
        )
      )
  )
(define (display-obj x)
  (displayln
   (if (eq? (car x) 'complex)
       (list 'complex (real-part (cdr x)) (imag-part (cdr x)))
       (if (eq? (car x) 'rational)
           (list (car x) (cadr x) (cddr x))
           (list (car x) (cdr x))
           )
       )
   )
  )
(install-integer-package)
(install-rational-package)
(install-complex-package)
(install-rectangular-package)
(install-polar-package)
(install-real-package)
(install-raise-package)


(define real-1 (make-real 3.0))
(define rational-1 (make-rational 4 3))
(define integer-1 (make-integer 2))
(define complex-1 (make-complex-from-real-imag 30 40))
(displayln "*****1")
(display-obj integer-1)
(display-obj real-1)
(display-obj rational-1)
(display-obj complex-1)

(displayln "*****2")
(display-obj ((get 'raise 'integer) integer-1)) ;interger converted to rational
(display-obj ((get 'raise 'rational) rational-1)) ;rational converted to real
(display-obj ((get 'raise 'real) real-1)) ;real converted to complex

(displayln "*****3")
(display-obj (add real-1 integer-1))
(display-obj (add real-1 rational-1))
(display-obj (add real-1 complex-1))
(display-obj (add complex-1 integer-1))
(display-obj (add integer-1 rational-1))

(displayln "******")

;a tagged complex is like: (cons 'complex (cons 'rectangular (cons 3 4))),displayed in racket as '(complex rectangular 3 . 4) 

(define (myloop)
  (define (make-obj lst)
    (let ((t (car lst)))
      (cond ((eq? t 'integer) (make-integer (cadr lst)))
            ((eq? t 'rational) (make-rational (cadr lst) (caddr lst)))
            ((eq? t 'real) (make-real (cadr lst)))
            ((eq? t 'complex) (make-complex-from-real-imag (cadr lst) (caddr lst))))))

  (let ((exp (read)))
    (if (eq? exp eof)
        (void)
        (let ((op (car exp))
              (a1 (make-obj (cadr exp)))
              (a2 (make-obj (caddr exp))))
          (display-obj (cond ((eq? op '+) (add a1 a2))
                ((eq? op '-) (sub a1 a2))
                ((eq? op '*) (mul a1 a2))
                ((eq? op '/) (div a1 a2))))
          (myloop)))))
(myloop)

by 1800012803(Lavintan)
#lang racket
(define (square x) (* x x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现，不须搞明白也能完成本题
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
;以上是put和get的实现，不须搞明白也能完成本题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;---------- about tags:
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

;--------- rectangular compex package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (void))

;----------- polar complex package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (void))



;----------- rational package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'rational 'numer
       (lambda (x) (numer (contents x)))) ; x is tagged rational
  (put 'rational 'denom
       (lambda (x) (denom (contents x)))) ; x is tagged rational

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (void))
(define (make-rational n d)
  ((get 'make 'rational) n d))




;---------- high level complex package
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
;  (define (mul-complex z1 z2)
;    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
;                       (+ (angle z1) (angle z2))))
;  (define (div-complex z1 z2)
;    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
;                       (- (angle z1) (angle z2))))
 (define (mul-complex z1 z2)
   (let ((a (real-part z1))
         (b (imag-part z1))
         (c (real-part z2))
         (d (imag-part z2)))
     (make-from-real-imag (- (* a c) (* b d)) (+ (* a d) (* b c)))))

 (define (div-complex z1 z2)
   (let ((a (real-part z1))
         (b (imag-part z1))
         (c (real-part z2))
         (d (imag-part z2)))
     (let ((denom (+ (square c) (square d))))
       (make-from-real-imag (/ (+ (* a c) (* b d)) denom)
                            (/ (- (* b c) (* a d)) denom)))))

  ;(a+bi)/(c+di) =(a+bi)*(c-di)/(c+di)*(c-di)=(ac-adi+bci+bd)/(c*c+d*d)=(ac+bd)/(c^2+d^2)+〔(bc-ad)/(c^2+d^2)〕i  
  
  
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (void))

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

;---------------- real package
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))    
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag (+ x 0.0))))
  (void))

(define (make-real x)
  ((get 'make 'real) x))


;----------- general functions
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))






(define (install-raise-package) ;install raise functions 
  (define (raise-integer n)
    (make-rational (contents n) 1))
  (define (raise-rational n)
    (make-real (/ (car (contents n))
                  (cdr (contents n)))))
  (define (raise-real n)
    ((get 'make-from-real-imag 'complex) (contents n) 0))

  (put 'raise 'integer raise-integer)
  (put 'raise 'rational raise-rational)
  (put 'raise 'real raise-real))

(define (query tag)
  (cond
    ((eq? tag 'integer) 0)
    ((eq? tag 'rational) 1)
    ((eq? tag 'real) 2)
    ((eq? tag 'complex) 3)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let(
            (type1 (car type-tags))
            (type2 (cadr type-tags))
            (a1 (car args))
            (a2 (cadr args))  )
            (if (< (query type1) (query type2))
              (apply-generic op ((get 'raise type1) a1) a2)
              (apply-generic op a1 ((get 'raise type2) a2))))
          (void))))))

(define (display-obj n)
  (define (display-iter n)
    (if (null? n)
      '()
      (if (pair? n)
        (if (eq? (car n) 'rectangular)
          (display-iter (cdr n))
          (cons (car n) (display-iter (cdr n))))
        (list n))))
  (displayln (display-iter n)))
(install-integer-package)
(install-rational-package)
(install-complex-package)
(install-rectangular-package)
(install-polar-package)
(install-real-package)
(install-raise-package)


(define real-1 (make-real 3.0))
(define rational-1 (make-rational 4 3))
(define integer-1 (make-integer 2))
(define complex-1 (make-complex-from-real-imag 30 40))
(displayln "*****1")
(display-obj integer-1)
(display-obj real-1)
(display-obj rational-1)
(display-obj complex-1)

(displayln "*****2")
(display-obj ((get 'raise 'integer) integer-1)) ;interger converted to rational
(display-obj ((get 'raise 'rational) rational-1)) ;rational converted to real
(display-obj ((get 'raise 'real) real-1)) ;real converted to complex

(displayln "*****3")
(display-obj (add real-1 integer-1))
(display-obj (add real-1 rational-1))
(display-obj (add real-1 complex-1))
(display-obj (add complex-1 integer-1))
(display-obj (add integer-1 rational-1))

(displayln "******")

;a tagged complex is like: (cons 'complex (cons 'rectangular (cons 3 4))),displayed in racket as '(complex rectangular 3 . 4) 

(define (myloop)
  (define (make-obj lst)
    (let ((t (car lst)))
      (cond ((eq? t 'integer) (make-integer (cadr lst)))
            ((eq? t 'rational) (make-rational (cadr lst) (caddr lst)))
            ((eq? t 'real) (make-real (cadr lst)))
            ((eq? t 'complex) (make-complex-from-real-imag (cadr lst) (caddr lst))))))

  (let ((exp (read)))
    (if (eq? exp eof)
        (void)
        (let ((op (car exp))
              (a1 (make-obj (cadr exp)))
              (a2 (make-obj (caddr exp))))
          (display-obj (cond ((eq? op '+) (add a1 a2))
                ((eq? op '-) (sub a1 a2))
                ((eq? op '*) (mul a1 a2))
                ((eq? op '/) (div a1 a2))))
          (myloop)))))
(myloop)

by 1800012751
#lang racket
(define (square x) (* x x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现，不须搞明白也能完成本题
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
;以上是put和get的实现，不须搞明白也能完成本题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;---------- about tags:
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

;--------- rectangular compex package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (void))

;----------- polar complex package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (void))



;----------- rational package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'rational 'numer
       (lambda (x) (numer (contents x)))) ; x is tagged rational
  (put 'rational 'denom
       (lambda (x) (denom (contents x)))) ; x is tagged rational

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (void))
(define (make-rational n d)
  ((get 'make 'rational) n d))




;---------- high level complex package
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
;  (define (mul-complex z1 z2)
;    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
;                       (+ (angle z1) (angle z2))))
;  (define (div-complex z1 z2)
;    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
;                       (- (angle z1) (angle z2))))
 (define (mul-complex z1 z2)
   (let ((a (real-part z1))
         (b (imag-part z1))
         (c (real-part z2))
         (d (imag-part z2)))
     (make-from-real-imag (- (* a c) (* b d)) (+ (* a d) (* b c)))))

 (define (div-complex z1 z2)
   (let ((a (real-part z1))
         (b (imag-part z1))
         (c (real-part z2))
         (d (imag-part z2)))
     (let ((denom (+ (square c) (square d))))
       (make-from-real-imag (/ (+ (* a c) (* b d)) denom)
                            (/ (- (* b c) (* a d)) denom)))))

  ;(a+bi)/(c+di) =(a+bi)*(c-di)/(c+di)*(c-di)=(ac-adi+bci+bd)/(c*c+d*d)=(ac+bd)/(c^2+d^2)+〔(bc-ad)/(c^2+d^2)〕i  
  
  
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (void))

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

;---------------- real package
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))    
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag (+ x 0.0))))
  (void))

(define (make-real x)
  ((get 'make 'real) x))


;----------- general functions
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))






(define (install-raise-package) ;install raise functions 
  (define (raise-integer n)
    (make-rational (contents n) 1))
  (define (raise-rational n)
    (make-real (/ ((get 'rational 'numer) n) ((get 'rational 'denom) n))))
  (define (raise-real n)
    (make-complex-from-real-imag (contents n) 0))
  (put 'raise 'integer raise-integer)
  (put 'raise 'rational raise-rational)
  (put 'raise 'real raise-real))

(define (P x)
  (cond ((eq? (type-tag x) 'integer) 0)
        ((eq? (type-tag x) 'rational) 1)
        ((eq? (type-tag x) 'real) 2)
        ((eq? (type-tag x) 'complex) 3)))

(define (apply-generic op . lst)
  (if (null? (cdr lst))
      ((get op (list (caar lst))) (cdar lst))
      (let ((x (car lst)) (y (cadr lst))
            (px (P (car lst))) (py (P (cadr lst))))
        (cond ((= px py) ((get op (list (type-tag x) (type-tag y))) (contents x) (contents y)))
              ((< px py) (apply-generic op ((get 'raise (type-tag x)) x) y))
              (else (apply-generic op x ((get 'raise (type-tag y)) y)))))))
  
(define (display-obj obj)
  (cond ((eq? (type-tag obj) 'integer) (displayln (list 'integer (contents obj))))
        ((eq? (type-tag obj) 'rational) (displayln (list 'rational ((get 'rational 'numer) obj) ((get 'rational 'denom) obj))))
        ((eq? (type-tag obj) 'real) (displayln (list 'real (contents obj))))
        ((eq? (type-tag obj) 'complex) (displayln (list 'complex (real-part (cdr obj)) (imag-part (cdr obj)))))))
(install-integer-package)
(install-rational-package)
(install-complex-package)
(install-rectangular-package)
(install-polar-package)
(install-real-package)
(install-raise-package)


(define real-1 (make-real 3.0))
(define rational-1 (make-rational 4 3))
(define integer-1 (make-integer 2))
(define complex-1 (make-complex-from-real-imag 30 40))
(displayln "*****1")
(display-obj integer-1)
(display-obj real-1)
(display-obj rational-1)
(display-obj complex-1)

(displayln "*****2")
(display-obj ((get 'raise 'integer) integer-1)) ;interger converted to rational
(display-obj ((get 'raise 'rational) rational-1)) ;rational converted to real
(display-obj ((get 'raise 'real) real-1)) ;real converted to complex

(displayln "*****3")
(display-obj (add real-1 integer-1))
(display-obj (add real-1 rational-1))
(display-obj (add real-1 complex-1))
(display-obj (add complex-1 integer-1))
(display-obj (add integer-1 rational-1))

(displayln "******")

;a tagged complex is like: (cons 'complex (cons 'rectangular (cons 3 4))),displayed in racket as '(complex rectangular 3 . 4) 

(define (myloop)
  (define (make-obj lst)
    (let ((t (car lst)))
      (cond ((eq? t 'integer) (make-integer (cadr lst)))
            ((eq? t 'rational) (make-rational (cadr lst) (caddr lst)))
            ((eq? t 'real) (make-real (cadr lst)))
            ((eq? t 'complex) (make-complex-from-real-imag (cadr lst) (caddr lst))))))

  (let ((exp (read)))
    (if (eq? exp eof)
        (void)
        (let ((op (car exp))
              (a1 (make-obj (cadr exp)))
              (a2 (make-obj (caddr exp))))
          (display-obj (cond ((eq? op '+) (add a1 a2))
                ((eq? op '-) (sub a1 a2))
                ((eq? op '*) (mul a1 a2))
                ((eq? op '/) (div a1 a2))))
          (myloop)))))
(myloop)

by 1800013090
#lang racket
(define (square x) (* x x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现，不须搞明白也能完成本题
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
;以上是put和get的实现，不须搞明白也能完成本题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;---------- about tags:
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

;--------- rectangular compex package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (void))

;----------- polar complex package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (void))



;----------- rational package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'rational 'numer
       (lambda (x) (numer (contents x)))) ; x is tagged rational
  (put 'rational 'denom
       (lambda (x) (denom (contents x)))) ; x is tagged rational

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (void))
(define (make-rational n d)
  ((get 'make 'rational) n d))




;---------- high level complex package
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
;  (define (mul-complex z1 z2)
;    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
;                       (+ (angle z1) (angle z2))))
;  (define (div-complex z1 z2)
;    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
;                       (- (angle z1) (angle z2))))
 (define (mul-complex z1 z2)
   (let ((a (real-part z1))
         (b (imag-part z1))
         (c (real-part z2))
         (d (imag-part z2)))
     (make-from-real-imag (- (* a c) (* b d)) (+ (* a d) (* b c)))))

 (define (div-complex z1 z2)
   (let ((a (real-part z1))
         (b (imag-part z1))
         (c (real-part z2))
         (d (imag-part z2)))
     (let ((denom (+ (square c) (square d))))
       (make-from-real-imag (/ (+ (* a c) (* b d)) denom)
                            (/ (- (* b c) (* a d)) denom)))))

  ;(a+bi)/(c+di) =(a+bi)*(c-di)/(c+di)*(c-di)=(ac-adi+bci+bd)/(c*c+d*d)=(ac+bd)/(c^2+d^2)+〔(bc-ad)/(c^2+d^2)〕i  
  
  
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (void))

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

;---------------- real package
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))    
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag (+ x 0.0))))
  (void))

(define (make-real x)
  ((get 'make 'real) x))


;----------- general functions
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))






(define (install-raise-package) ;install raise functions 
  (define (raise-integer n)
    (make-rational (contents n) 1))
(define (raise-rational n)
    (make-real (exact->inexact (/ (car (contents n)) (cdr (contents n))))))
  (define (raise-real n)
    (make-complex-from-real-imag (contents n) 0))
  (put 'raise 'integer (lambda (n) (raise-integer n)))
  (put 'raise 'rational (lambda (n) (raise-rational n)))
  (put 'raise 'real (lambda (n) (raise-real n))))

(define (apply-generic op-tag x . Y)
  (define (next-tag tag)
    (cond  ((eq? tag 'integer) 'rational)
           ((eq? tag 'rational) 'real)
           ((eq? tag 'real) 'complex)
           ((eq? tag 'complex) 'maxx)))
  (define (<? tag1 tag2)
    (if (eq? tag1 tag2)
        #t
        (if (eq? tag1 'maxx)
            #f
            (<? (next-tag tag1) tag2))))
  (if (pair? Y)
      (let ((y (car Y)))
        (if (eq? (type-tag x) (type-tag y))
            ((get op-tag (list (type-tag x) (type-tag y))) (contents x) (contents y))
            (if (<? (type-tag x) (type-tag y))
                (apply-generic op-tag ((get 'raise (type-tag x)) x) y)
                (apply-generic op-tag x ((get 'raise (type-tag y)) y)))))
      ((get op-tag (list (type-tag x))) (contents x))))

(define (display-obj n)
  (let ((t (type-tag n))
        (c (contents n)))
    (cond ((eq? t 'integer) (printf "(integer ~s)\n" c))
          ((eq? t 'rational) (printf "(rational ~s ~s)\n" (car c) (cdr c)))
          ((eq? t 'real) (printf "(real ~s)\n" c))
          ((eq? t 'complex) (printf "(complex ~s ~s)\n" (cadr c) (cddr c))))))
(install-integer-package)
(install-rational-package)
(install-complex-package)
(install-rectangular-package)
(install-polar-package)
(install-real-package)
(install-raise-package)


(define real-1 (make-real 3.0))
(define rational-1 (make-rational 4 3))
(define integer-1 (make-integer 2))
(define complex-1 (make-complex-from-real-imag 30 40))
(displayln "*****1")
(display-obj integer-1)
(display-obj real-1)
(display-obj rational-1)
(display-obj complex-1)

(displayln "*****2")
(display-obj ((get 'raise 'integer) integer-1)) ;interger converted to rational
(display-obj ((get 'raise 'rational) rational-1)) ;rational converted to real
(display-obj ((get 'raise 'real) real-1)) ;real converted to complex

(displayln "*****3")
(display-obj (add real-1 integer-1))
(display-obj (add real-1 rational-1))
(display-obj (add real-1 complex-1))
(display-obj (add complex-1 integer-1))
(display-obj (add integer-1 rational-1))

(displayln "******")

;a tagged complex is like: (cons 'complex (cons 'rectangular (cons 3 4))),displayed in racket as '(complex rectangular 3 . 4) 

(define (myloop)
  (define (make-obj lst)
    (let ((t (car lst)))
      (cond ((eq? t 'integer) (make-integer (cadr lst)))
            ((eq? t 'rational) (make-rational (cadr lst) (caddr lst)))
            ((eq? t 'real) (make-real (cadr lst)))
            ((eq? t 'complex) (make-complex-from-real-imag (cadr lst) (caddr lst))))))

  (let ((exp (read)))
    (if (eq? exp eof)
        (void)
        (let ((op (car exp))
              (a1 (make-obj (cadr exp)))
              (a2 (make-obj (caddr exp))))
          (display-obj (cond ((eq? op '+) (add a1 a2))
                ((eq? op '-) (sub a1 a2))
                ((eq? op '*) (mul a1 a2))
                ((eq? op '/) (div a1 a2))))
          (myloop)))))
(myloop)

****3: 巧妙的continuation
by 1800012802
#lang racket

(define exit #f)

(define cont-list '())
(define len (read))

(define (set-cont-list n)

  (define (print n)
    (if(= n 0)(void)
       (begin(displayln n)(print (- n 1)))))
  (if(= n 0)(void)
     (if(call/cc (lambda (k)(begin (set! cont-list (cons k cont-list))#t)))(set-cont-list (- n 1))
        (print n))))
(define (show n)
  (define (show-helper l n)
    (if (= n 0)
        (if (continuation? (car l))
            ((car l) #f)
            (displayln "error"))
        (show-helper (cdr l) (- n 1))))
  (show-helper cont-list (- n 1)))

(define (main)
  (set-cont-list len)
  (define k (read))
  (if (eq? k eof)
      (void)
      (show k)))

(main)

by 1800012751
#lang racket

(define exit #f)

(define cont-list '())
(define len (read))

(define (set-cont-list n)
  (if (call/cc (lambda (c) (set! cont-list (cons c cont-list)) #t))
      (if (= n 1)
          (void)
          (set-cont-list (- n 1)))
      (if (= n 1)
          (displayln n)
          (begin (displayln n) (show (- n 1))))))

(define (show n)
  (define (show-helper l n)
    (if (= n 0)
        (if (continuation? (car l))
            ((car l) #f)
            (displayln "error"))
        (show-helper (cdr l) (- n 1))))
  (show-helper cont-list (- n 1)))

(define (main)
  (set-cont-list len)
  (define k (read))
  (if (eq? k eof)
      (void)
      (show k)))

(main)

by 1700013005
#lang racket

(define exit #f)

(define cont-list '())
(define len (read))

(define (set-cont-list n)
  (define (print n) (if (= n 0) (void) (begin (displayln n) (print (- n 1)))))
  (if (= n 0)
      (void)
      (if (call/cc (lambda (c) (set! cont-list (cons c cont-list)) (c #t)))
          (set-cont-list (- n 1))
          (print n))))
(define (show n)
  (define (show-helper l n)
    (if (= n 0)
        (if (continuation? (car l))
            ((car l) #f)
            (displayln "error"))
        (show-helper (cdr l) (- n 1))))
  (show-helper cont-list (- n 1)))

(define (main)
  (set-cont-list len)
  (define k (read))
  (if (eq? k eof)
      (void)
      (show k)))

(main)

by 1600012986
#lang racket

(define exit #f)

(define cont-list '())
(define len (read))

(define (set-cont-list n)
  (if (call/cc (lambda (c)
                 (set! cont-list (cons c cont-list))
                 #t))
      (unless (= 1 n) (set-cont-list (sub1 n)))
      (let loop ([i n])
        (displayln i)
        (unless (= 1 i) (loop (sub1 i))))))
(define (show n)
  (define (show-helper l n)
    (if (= n 0)
        (if (continuation? (car l))
            ((car l) #f)
            (displayln "error"))
        (show-helper (cdr l) (- n 1))))
  (show-helper cont-list (- n 1)))

(define (main)
  (set-cont-list len)
  (define k (read))
  (if (eq? k eof)
      (void)
      (show k)))

(main)

