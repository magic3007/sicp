#lang racket

(define (average a b)
  (/ (+ a b) 2))

(define threshold 1e-6)

(define (close-enough? a b)
  (< (abs (- a b)) threshold))

(define (cube x)
  (* x x x))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmp (lambda (x)
                (map (lambda (p) (cons x p))
                     (permutations (remove x s))))
              s)))

(define (cartesian-product . seqs)
  (if (null? seqs)
      (list '())
      (let ((tmp (apply cartesian-product (cdr seqs))))
        (flatmap (lambda (x)
                   (map (lambda (e) (cons x e))
                        tmp))
                 (car seqs)))))

(define (print-line lst)
    (for-each (lambda (x) (display x) (display " ")) lst))

; ==========================================
; buildin procedure
; More info in: https://docs.racket-lang.org/reference/
; ==========================================

#|

3.9 Local Binding
  
  let* :  evaluates the val-exprs **one by one**
    > (let* ([x 1]
         [y (+ x 1)])
      (list y x))
    '(2 1)

  letrec :  all ids are created first.
    >(letrec ([is-even? (lambda (n)
                       (or (zero? n)
                           (is-odd? (sub1 n))))]
           [is-odd? (lambda (n)
                      (and (not (zero? n))
                           (is-even? (sub1 n))))])
    (is-odd? 11))

  (let-values ([(id ...) val-expr] ...) body ...+): return multiple values
  > (let-values ([(x y) (quotient/remainder 10 3)])
    (list y x))
    '(1 3)
  let*-values
  letrec-values

3.13 Dispatch:
  (case val-expr
    [(datum1...) then-body1 ...+]
    [(datum2...) then-body2 ...+]
    [else else-body ...+]

4.2.1 Number Types
  even? odd?
  zero? positive? negative?

4.2.2.1 Arthmetic
  quotient
  (remainder n m): return the same sign as n
  (modulo n m): return the same sign as m
  (quotient/remainder n m) → (values (quotient n m) (remainder n m))
  add1
  sub1
  abs
  max min
  gcd lcm
  round : Returns the integer closest to x:real
  floor ceil truncate
  exact-round exact-floor exact-ceiling exact-truncate: return exact integer
  (numerator q) (denominator q): Coerces q to an exact number,  finds the numerator
     or demominatorof the number expressed in its simplest fractional form
  (rationalize x tolerance): Among the real numbers within (abs tolerance) of x, returns the one
     corresponding to an exact number whose denominator is the smallest.
    > (rationalize 1/4 1/10)
      1/3

4.2.2.10 Extra constants and functions
   pi pi.f
   degrees->radians radians->degree
   sqr
   sgn
   conjugate

4.2.2.3 Powers and Roots
  sqrt
  (integer-sqrt n) = (floor (sqrt n))
  (integer-sqrt/remainder n)
  (expt z w): return z raised to the power of w
  (exp z): return e^z
  (log z [b])

4.2.2.4 Trigonometric Functions
  sin cos tan asin acos atan

4.2.2.5 Complex Numbers
 (make-rectangular x y): return x+yi
 (make-polar magnitude angle)
 real-part image-part magnitude angle

4.2.2.6 Bitwise Operations

 bitwise-ior / bitwise-and / bitwise-xor / bitwise-not
 bitwise-bit-set?
  

4.9.1 Pair Constructors and Selectors
  list*:  the last argument is used as the tail of 
    the result, instead of the final element.
  > (list* 1 2)
    '(1 . 2)
  > (list* 1 2 (list 3 4))
    '(1 2 3 4)

  (build-list n proc): Creates a list of n elements by applying proc 
    to the integers from 0 to (sub1 n) in order
    > (build-list 10 values)
      '(0 1 2 3 4 5 6 7 8 9)
    > (build-list 5 (lambda (x) (* x x)))
      '(0 1 4 9 16)

4.9.2 List Operations
  list-tail :
    > (list-tail (list 1 2 3 4 5) 2) 
    '(3 4 5)

  (append list ...)

  (reverse list)

4.9.3 List Iteration
  map
  andmap ormap
    > (andmap positive? '(1 2 3))
      #t
  for-each: similar to map, but its result is ignored
  (foldl proc init lst ...+): The proc is initially invoked with the 
      first item of each list, and the final argument is init.
    > (foldl + 0 '(1 2 3 4))
      10
    > (foldl (lambda (a b result)
           (* result (- a b)))
         1
         '(1 2 3)
         '(4 5 6))
      -27
  foldr: Like foldl, but the lists are traversed from right to left

4.9.4 List Filtering
  filter
  (remove v lst [proc])
  (remq v lst) = (remove v lst eq?)
  remv 
  (remove* v-lst lst [proc])
  remq*
  remv*
  sort
    > (sort '(1 3 4 2) <)
      '(1 2 3 4)

4.9.5 List Searching
  member: Locates the first element of lst that is equal? to v, and return
    the tail of lst starting with that element
  memv: eqv?
  memq: eq?
  (memf proc lst):  finds an element using the predicate proc
  findf: Like memf, but returns the element found

4.9.6 Pair Accessor Shorthands
  make-list:
    > (make-list 7 'foo)
    '(foo foo foo foo foo foo foo)

4.17 Procedures
  apply: list -> multiple parameters

10.1 Multiple Values
  (values v ...):
    > (values 1 2 3)
      1
      2
      3
  (call-with-values generator receiver):
    > (call-with-values (lambda () (values 1 2)) +)
      3



4.17.3 Additional Higher-Order Functions
  
  negate: it returns the not of proc’s result

  conjoin, disjoin: combines calls to each functions with and / or.
  > (define f (conjoin exact? integer?))

  (curry proc v ...+): eturns a procedure that is a curried version of proc

|#