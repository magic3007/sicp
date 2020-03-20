#lang racket


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

(define (apply-generic op . args)
  (let* ([type-tags (map type-tag args)]
         [proc (get op type-tags)])
    (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
            (let* ([type1 (car type-tags)]
                  [type2 (cadr type-tags)]
                  [a1 (car args)]
                  [a2 (cadr args)]
                  [t1->t2 (get-coercion type1 type2)]
                  [t2->t1 (get-coercion type2 type1)])
              (cond [t1->t2
                     (apply-generic op (t1->t2 a1) a2)]
                    [t2->t1
                     (apply-generic op a1 (t2->t1 a2))]
                    (else
                     (error
                      "No method for these types -- APPLY-GENERIC"
                      (list op type-tags)))))
             (error
                      "No method for these types -- APPLY-GENERIC"
                      (list op type-tags))))))

; pass 1: install package
(define (install-rectangular-package)
  ; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (sqr (real-part z))
             (sqr (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (=zero? z) (and (zero? (real-part z)) (zero? (imag-part z))))
 ; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) ; list
       real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put '=zero? '(rectangular) =zero?)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular ; single item
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ; internal procedure
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (=zero? z) (zero? (magnitude z)))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (sqr x) (sqr y)))
          (atan y x)))
  ; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put '=zero? '(polar) =zero?)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'ploar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put '=zero? '(scheme-number) zero?)
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (install-rational-package)
  ; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ([g (gcd n d)])
      (cons (/ n g) (/ d g))))
  (define (=zero? x) (zero? (numer x)))
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
  ; interface
  (define (tag x) (attach-tag 'rational x))
  (put '=zero? '(rational) zero?)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ; internal procedures
  (define (=zero? z) (and (zero? (real-part z)) (zero? (imag-part z))))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ; across different types
  (define (add-complex-to-schemenum z x)
    (make-from-real-imag (+ (real-part z) x)
                         (imag-part z)))
  ; type coercion
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  
  ; interface
  (define (tag z) (attach-tag 'complex z))
  (put '=zero? '(complex) =zero?)
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'add '(complex scheme-number)
       (lambda (z x) (tag (add-complex-to-schemenum z x))))
  (put 'make-from-real-imag 'complex
       (lambda (r a) (tag (make-from-real-imag r a))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  'done)

  
; pass 2: exterior function

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (make-ratioal n d)
  ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (=zero? z) (apply-generic '=zero? z))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-complex-package)

; polynomial exporession pattern: 
; (polynomial x (2 (rational 4 . 1)) (1 (rational 6 . 1)) (0 (rational 2 . 1)))
; = 4x^2+6x+2


(define (install-polynomial-package)

  ; internal procedures

  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  
  ; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))

  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (adjoin-term term term-list)
    ; generic zero
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (first term))
  (define (coeff term) (second term))
  
  (define (add-terms L1 L2)
    (cond [(empty-termlist? L1) L2]
          [(empty-termlist? L2) L1]
          (else
           (let* ([t1 (first-term L1)]
                  [t2 (first-term L2)])
             (cond [(> (order t1) (order t2))
                   (adjoin-term
                    t1 (add-terms (rest-terms L1) L2))]
                   [(< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2)))]
                   [else
                    (adjoin-term
                     (make-term (order t1)
                                 ; (define (add x y) (apply-generic 'add x y))
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1) (rest-terms L2)))])))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ([t2 (first-term L)])
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                       ; (define (mul x y) (apply-generic 'mul x y))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
                      
          
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
          
          
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  ; interface
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-polynomial-package)

(mul (make-polynomial 'x (list (list 2 (make-scheme-number 1)) (list 0 (make-scheme-number 1))))
     (make-polynomial 'x (list (list 2 (make-scheme-number 1)) (list 0 (make-scheme-number 1)))))