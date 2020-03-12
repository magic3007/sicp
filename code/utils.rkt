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

; the same as append-map
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
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

(define (join lst delimiter)
  (rest (append* (map (lambda (x) (list delimiter x))
                    lst))))

(define (printListln lst delimiter)
  (for-each (lambda (x) (display x)) (join lst delimiter))
  (newline))


; ==========================================
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
  let-values: (let-values ([(id ...) val-expr] ...) body ...+) eturn multiple values
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
  -- list*:  the last argument is used as the tail of 
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
  list-tail: (list-tail lst pos) suffix operation. we also have prefix operation (take lst pos)
              > (list-tail (list 1 2 3 4 5) 2) 
                '(3 4 5)
  append: (append list ...)  the result is a list that contains all of the elements of the given
          lists in order. 
          > (append (list 1 2) (list 3 4))
          '(1 2 3 4)
  append*: (append* lst ... lsts) = (apply append lst ... lsts) Like append, but the last argument
           is used as a list of arguments for append.
           Note that we just have one argument. in this way, is to **unwrap+connect** a list
           (append* '((1 2) (3 4))) -> '(1 2 3 4)
         > (append* '(a) '(b) '((c) (d)))
         '(a b c d)
         > (cdr (append* (map (lambda (x) (list ", " x))
                     '("Alpha" "Beta" "Gamma"))))
           '("Alpha" ", " "Beta" ", " "Gamma")
  flatten: (flatten v) flattens an arbitrary S-expression structure
           of pairs into a list. It could also change a single value into a list.
           > (flatten '((a) b (c (d) . e) ()))
           '(a b c d e)
           > (flatten 'a)
           '(a)

  reverse: (reverse list) reverse the list 
      
  
4.9.3 List Iteration
  map: (map proc lst ...+)
  append-map: (append-map proc lst ...+) = (append* (map proc list ...))
               the same as flatmap!!!!
  filter-map: (filter-map proc lst ...+) = (filter (lambda (x) x) (map proc lst ...))
               Like (map proc lst ...), except that, if proc returns #false, that element is omitted
  count: (count proc list ...+) = (length (filter-map proc list ...))

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
  filter: (filter pred lst)
  remove: (remove v lst [proc])
    remq: (remq v lst) = (remove v lst eq?)
    remv: (remv v lst) = (remove v list eqv?) 
  remove*, remq*, remv*: (remove* v-lst lst [proc]) like remove, but remove from list
           every instance of every element of v-list
           Example:
           > (remove* (list 1 2) (list 1 1 3 2 4 5 2)
             `(3 4 5)
  sort: (sort lst less-than? [	#:key extract-key #:cache-keys? cache-keys?]) → list?
    > (sort '(1 3 4 2) <)
      '(1 2 3 4)
    > (sort '("aardvark" "dingo" "cow" "bear") string<?)
      '("aardvark" "bear" "cow" "dingo")
    > > (sort '(("aardvark") ("dingo") ("cow") ("bear"))
           #:key car string<?)
        '(("aardvark") ("bear") ("cow") ("dingo"))

4.9.5 List Searching
  member: (member v lst [is-equal?]) Locates the first element of lst that is equal? to v, and return
           the tail of lst starting with that element
    memq: (memv v lst) = (member v lst eq?)
    memv: (memv v lst) = (member v lst eqv?)
  memf: (memf proc lst) like member, but finds an element using the predicate proc
  findf: (findf proc lst) Like memf, but returns the element found
  assoc: (assoc v lst [is-equal?]) locate the first element of lst whose |car| is equal to v
         > (assoc 3 (list (list 1 2) (list 3 4) (list 5 6)))
           '(3 4)
    assv: (assv v lst) = (assoc v lst eqv?)
    assq: (assq v lst) = (assoc v lst eq?)
  assf: (assf proc lst): similar to assoc, but return an element using the predicate |proc|
    > (assf (lambda (arg)
          (> arg 2))
        (list (list 1 2) (list 3 4) (list 5 6)))
       '(3 4)



4.9.7 Additional List Functions and Synonyms
  null? cons?
  first: The same as (car lst), but only for lists (that are not empty).
  rest: The same as (cdr lst), but only for lists (that are not empty).
  last: (last lst) return the last element of the list
        > (last '(1 2 3 4 5 6 7 8 9 10))
          10
  last-pair: (last-pair p) Returns the last pair of a (possibly improper) list.
        > (last-pair '(1 2 3 4))
          '(4)
  make-list: (make-list k v) Returns a newly constructed list of length k, holding v in all positions.
        > (make-list 7 'foo)
       '(foo foo foo foo foo foo foo)
  list-update: (list-update lst pos updater) Returns a list that is the same as lst except at the
               specified index. The element at the specified index is (updater (list-ref lst pos)).
        > (list-update '(zero one two) 1 symbol->string)
          '(zero "one" two)
   list-set: (list-set lst pos value) Returns a list that is the same as lst except at the specified
              index. The element at the specified index is value.

   index-of: (index-of lst v [is-equal?]) similar to member, but return the index
   indexes-of: (indexes-of lst v [is-equal?]) like index-of but return all the indexs as list
   index-where: (index-where lst proc) like index-of, but with predicate-searching
               > (index-where '(1 2 3 4) even?)
   indexes-where:  like index-where but return all the indexs as list            1

   take: (take lst pos)  (prefix operation)Returns a fresh list whose elements are the first pos elements of lst.
   drop: (drop lst pos)  (suffix operation) like list-tail Returns the list after the first pos elements of lst.
   split-at: (split-at lst pos) = (values (take lst pos) (drop lst pos))
   takef: (takef lst pred) similar to filter. Returns a fresh list whose elements are taken successively from lst as
                           long as they satisfy pred. 
   dropf: (dropf lst pred) Drops elements from the front of lst as long as they satisfy pred.
                           could be implemented by filters
   splitf-at: (splitf-at lst pred) = (values (takef lst pred) (dropf lst pred))
   check-duplicates: (check-duplicates lst [same? #:key extract-key #:default failure-result])
                   Returns the first duplicate item in lst; otherwise #:default
        eg. check if two list begin with the same ele
        > (check-duplicates '((a 1) (b 2) (a 3)) #:key car)
        > '(a 3)
    remove-duplicates: (remove-duplicates lst [same? #:key extract-key])
        Returns a list that has all items in lst, but without duplicate items
    range: (range end) or (range start end [step]) return a list of range
    
4.17 Procedures
  apply: (apply proc v ... lst #:<kw> kw-arg ...)
          the last argument is used as a list of arguments for append. We could also have exactly one
          arguments, in thi way, we could take a list of arguments for append


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