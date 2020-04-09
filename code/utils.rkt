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

(define (display-list lst delimiter)
  (for-each (lambda (x) (display x)) (join lst delimiter)))

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

; ==========================================
; More info in: https://docs.racket-lang.org/reference/
; ==========================================

#|

1.3.8 Reading Quotes
  ' quote 
  ` quasiquote
  , unquote

  `(1 ,2) reads equal to  (list 'quasiquote (list 1 (list 'unquote 2)))

3.9 Local Binding
    
  let : (let proc-id ([id init-expr] ...) body ...+)
    let is a syntactic sugar of lambda. *proc-id* could specify the function name.
    (define (kproduct lst k)
      (let ([break k])
        (let loop ([lst lst] [k k])
          (cond
            [(null? lst) (k 1)]
            [(zero? (first lst)) (break 0)]
            [else (loop (cdr lst) (lambda (x) (k (* (first lst) x))))]))))
 
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
  let-values: (let-values ([(id ...) val-expr] ...) body ...+) return multiple values
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

3.16 Guarded Evaluation: when and unless
    when: (when test-expr body ...+)
        similar to "if", but without else
    unless: (unless test-expr body ...+)
        similar to "if", but without then

3.15 Sequencing
    begin: (begin ...) the results are ignored for all but the last expr
    begin0: (begin0 ...) the results are ignored for all but the first expr

3.18 Iterations and Comprehensions
    - for: (for (for-clause ...) body-or-break ... body)
        for-clause      =       [id seq-expr]
                        |       [(id ...) seq-expr]
                        |       #:when guard-expr
                        |       #:unless guard-expr
                        |       break-clause
            The for form iterates by drawing an element from each sequence
             if any sequence is empty, then the iteration stops.
            However, If any for-clause has the form #:when guard-expr, then only the preceding 
            clauses (containing no #:when or #:unless) determine iteration as above
            
        > (for ([i '(1 2 3)]
                [j "abc"]
                [k #(#t #f)])
                (display (list i j k)))
            (1 a #t)(2 b #f)

        > for ([i '(1 2 3)]
                [j "abc"]
                #:when (odd? i)
                [k #(#t #f)])
                (display (list i j k)))
            (1 a #t)(1 a #f)(3 c #t)(3 c #f)(

        > (for ([i 3])
            (display i))
        012

        > (for ([i (in-range 3)])
            (display i))
        012

        > (for ([i (in-range 1 4)])
            (display i))
        123

        > (for ([i (in-range 1 4 2)])
            (display i))
        13

    - for/sum: (for/sum (for-clause ...) body-or-break ... body)
        Iterates like for, but each result of the last body is accumulated into a result with +.
        > (for/sum ([i '(1 2 3 4)]) i) 
        10

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
  filter-not: (filter-not pred lst) Like filter, but the meaning of the pred predicate is reversed
  remove: (remove v lst [proc]) omitting the first element of lst that is equal to v using the comparison procedure proc
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
  member: (member v lst [is-equal?]) Locates the first element of lst that is equal? to v, If such an element exists,
                                     **the tail of lst starting with that element is returned**. Otherwise, the result is #f.
    memq: (memv v lst) = (member v lst eq?)
    memv: (memv v lst) = (member v lst eqv?)
  memf: (memf proc lst) like member, but finds an element using the predicate proc’s
  findf: (findf proc lst) Like memf, but returns the element found
  assoc: (assoc v lst [is-equal?]) locate the first element of lst whose |car| is equal to v. Otherwise, the result is #f
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
 
4.10 Mutable Pairs & lists

```racket
(require scheme/mpair)
(define list mlist)
(define cdr mcdr)
(define car mcar)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
(define cons mcons)
(define assoc massoc)
```

4.11 Vectors

    - make-vector : (make-vector size [v=0])
    - vector : (vector ...) Returns a newly allocated mutable vector with as many slots as provided
    - build-vector: (build-vector n proc) Creates a vector of n elements by applying proc to the integers from 0 to (sub1 n)
    - vector-immuatble : (vector-immuatble ...) returns a newly allocated immutable vector
    - vector-length : (vector-length vec)
    - vector-ref : (vector-ref vec pos)
    - vector-set! : (vector-set! vec pos v)
    - vector->list : (vector->list vec)
    - list->vector : (list->vector lst)
    - vector->immutable-vector: (vector->immutable-vector vec)
    - vector-fill!: (vector-fill! vec v) Changes all slots of vec to contain v.
  
4.11.1 Additional Vector Functions
    
     (require racket/vector)

     - vector-empty? : (vector-empty? v)
     - vector-argmin : (vector-argmin proc vec) This returns the first element in the non-empty vector vec that minimizes the result of proc.
     - vector-argmax : (vector-argmax proc vec)
     - vector-sort: (vector-sort vec less-than? [start end #:key    #:cache-keys? cache-keys?])
        > (define v1 (vector 4 3 2 1))
        > (vector-sort v1 <)
        '#(1 2 3 4)

        > v1
        '#(4 3 2 1)

        > (define v2 (vector '(4) '(3) '(2) '(1)))
        > (vector-sort v2 < 1 3 #:key car)
        '#((2) (3))

        > v2
        '#((4) (3) (2) (1))
     - vector-sort!: (vector-sort! vec less-than? [start end #:key    #:cache-keys? cache-keys?])
        inplace.
        > (define v1 (vector 4 3 2 1))
        > (vector-sort! v1 <)
        > v1
        '#(1 2 3 4)

        > (define v2 (vector '(4) '(3) '(2) '(1)))
        > (vector-sort! v2 < 1 3 #:key car)
        > v2
        '#((4) (2) (3) (1))

4.13 Hash Tables
    make-hash: (make-hash [assocs])
        Creates a mutable hash table that holds keys strongly.
    hash-set!: (hash-set! hash key v)    
    hash-set*!: (hash-set*! hash key v ... ...)
        Maps each key to each v in hash
    hash-ref: (hash-ref hash key [failure-result])
        Returns the value for key in hash.
    hash-ref-key: (hash-ref-key hash key [failure-result])
        Returns the key held by hash that is equivalent to key according to hash’s key-comparison function.

    > (define original-key "hello")
    > (define key-copy (string-copy original-key))
    > (equal? original-key key-copy)
    #t
    > (eq? original-key key-copy)
    #f
    > (define table (make-hash))
    > (hash-set! table original-key 'value)
    > (eq? (hash-ref-key table "hello") original-key)
    #t
    > (eq? (hash-ref-key table "hello") key-copy)
    #f

4.17 Procedures
  apply: (apply proc v ... lst #:<kw> kw-arg ...)
          the last argument is used as a list of arguments for append. We could also have exactly one
          arguments, in thi way, we could take a list of arguments for append
  compose: (compose proc ...)
            Returns a procedure that composes the given functions, applying the last proc first and the first proc last.
            while compose1 restricts the internal value passing to a single value.
           > ((compose1 - sqrt) 10)
            -3.1622776601683795 
4.17.3 Additional Higher-Order Functions
  
  negate: it returns the not of proc’s result

  conjoin, disjoin: combines calls to each functions with and / or.
        > (define f (conjoin exact? integer?))

  curry: (curry proc v ...+) returns a procedure that is a curried version of proc
        > (define sum? (curry member '+))
  curryr: (curryr proc v ...+) Like curry, except that the arguments are collected in the opposite direction
        > (define in? (curryr member lst))

5.1 Struct
    (struct id maybe-supper (field ...)
        struct-option ...)
    maybe-supper = | supper-id
    field = field-d | [field-id field-option ...]
    struct-option = 
        #:mutable
        #:super
    field-option =
        #:mutable
        #:auto    


    ```racket
    (define-struct student (name [gpa #:mutable]))
    (define s (student "hello" 4.0))
    (student-name s)
    (set-student-gpa! s 3.9)

    (match s
      [(student name gpa) gpa]
      [_ "error"])
    ```

9. Pattern Matching
  match: (match val-expr clause ...)
    clause    =   [pat body ...+]
              |   [pat (=> id) body ...+]
              |   [pat #:when cond-expr body ...+]
    - An optional #:when cond-expr specifies 
    that the pattern should only match if cond-expr p
    roduces a true value.
    > (define (m x)
        (match x
          [(list a b c)
          #:when (= 6 (+ a b c))
          'sum-is-six']
        [(list a b c) 'sum-is-not-six']))
    > (m '(1 2 3))
     'sum-is-six
    > (m '(2 3 4))
    'sum-is-not-six
    - An optional (=> id) between a pat and the bodys is bound to a failure 
    procedure of zero arguments. If this procedure is invoked, it escapes back to the pattern matching expression, 
    and resumes the matching process as if the pattern had failed to match. 
    > (define (m x)
        (match x
          [(list a b c)
           (=> exit)
           (f x exit)]
          [(list a b c) 'sum-is-not-six]))
    > (define (f x exit)
        (if (= 6 (apply + x))
            'sum-is-six
            (exit)))
    > (m '(1 2 3))
     'sum-is-six
    > (m '(2 3 4))
    'sum-is-not-six
     - If an id is used multiple times within a pattern, 
     the corresponding matches must be the same according 
     to (match-equality-test), except that instances of an 
     id in different or and not sub-patterns are independent.
     > (match '(1 2 3)
       [(list a b a) (list a b)]
      [(list a b c) (list c b a)])
      '(3 2 1)
      _ : matches anything, without binding any identifiers.
      > (match '(1 2 3)
        [(list _ _ a) a])
        3

      - For spliced lists, ... and ___ are aliases for zero or more matches.
      The ..k and __k forms are also aliases, specifying k or more matches. 
      > (match '(1 2 3)
        [(list 1 a ...) a])
        '(2 3)
      > (match '(1 2 3)
        [(list 1 a ..3) a]
        [_ 'else])
        'else
      > (match '(1 2 3 4 5)
        [(list 1 a ..3 5) a]
        [_ 'else])
        '(2 3 4)
      > (match '(1 (2) (2) (2) 5)
        [(list 1 (list a) ..3 5) a]
        [_ 'else])
        '(2 2 2)

      - (list-rest lvp ... pat) —similar to a list pattern, but the 
      final pat matches the “rest” of the list after the last lvp.

        > (match '(1 2 3 . 4)
        [(list-rest a b c d) d])
        4
        > (match '(1 2 3 . 4)
          [(list-rest a ... d) (list a d)])
        '((1 2 3) 4)

      - (list-no-order pat ...) —similar to a list pattern, but the elements to match each 
      pat can appear in the list in any order.
      > (match '(1 2 3)
       [(list-no-order 3 2 x) x])
       1

      - (list-no-order pat ... lvp) —generalizes list-no-order to 
      allow a pattern that matches multiple list elements that are interspersed in any order with matches for the other patterns.
      > (match '(1 2 3 4 5 6)
        [(list-no-order 6 2 y ...) y])
        '(1 3 4 5)

      - (vector lvp ...) — like a list pattern, but matching a vector.
      > (match #(1 (2) (2) (2) 5)
        [(vector 1 (list a) ..3 5) a])
        '(2 2 2)

      - (hash-table (pat pat) ...) —similar to list-no-order, 
         but matching against hash table’s key–value pairs.
         > (match #hash(("a" . 1) ("b" . 2))
           [(hash-table ("b" b) ("a" a)) (list b a)])
           '(2 1)
      - (hash-table (pat pat) ...+ ooo)
         Generalizes hash-table to support a final repeating pattern
         > (match #hash(("a" . 1) ("b" . 2))
          [(hash-table (key val) ...) key])
          '("b" "a")
      - (cons a b) : matches a pair value
      - (mcons a b): matches a mutable pair value
      - (? expr pat ...): applies expr to the value to be matched, and checks whether the result is a true value
        > (match '(1 3 5)
            [(list (? odd?) ...) 'yes])
            'yes
  match*: (match* (val-expr ..+) clause* ...)
      Matches a sequence of values against each clause in order, matching only when all patterns in a clause match.

  match-let: (match-let ([pat expr]...) body...+)
              Generalizes let to support pattern bindings. 
            > (match-let ([(list a b) '(1 2)]
              [(vector x ...) #(1 2 3 4)])
                 (list b a x))
            '(2 1 (1 2 3 4))
            > (match* (1 2 3)
            [(_ (? number?) x) (add1 x)])
            4

            > (match* (15 17)
                [((? number? a) (? number? b))
                #:when (= (+ a 2) b)
                'diff-by-two])
                'diff-by-two
  match-let*: (match-let* ([pat expr]...) body...+)
            Like match-let, but generalizes let*, so that the 
            bindings of each pat are available in each subsequent expr.
  match-let-values: (match-let-values ([(pat ...) expr] ...) body ...+)
            Like match-let, but generalizes let-values.
  match-let*-values: (match-let*-values ([(pat ...) expr] ...) body ...+)
            Like match-let*, but generalizes let*-values.

10.1 Multiple Values
  (values v ...):
    > (values 1 2 3)
      1
      2
      3
  (call-with-values generator receiver):
    > (call-with-values (lambda () (values 1 2)) +)
      3

10.4 Continuations
    call/cc : (call/cc proc [prompt-tag])
        proc : (continuation? . -> . any)
        prompt-tag      : continuation-prompt-tag?
                          = (default-continuation-prompt-tag)
        alias for "call-with-current-continuation"
        - *call/cc* is a procedure of one argument *proc*, 
          and the aurgment *proc* must also be a procedure of one argument
        - the continuation is passed to *proc* as the parameter
        - *proc* is called directly in tail position with respect to *call/cc*.
          and its return value is return by *call/cc*
        - prompt-tag(optional): captures the current continuation up to the nearest prompt tagged by *prompt-tag*
    let/cc : (let/cc k body ...+) = (call/cc (lambda (k) body ...))

    
11.3.2 Parameters
    Parameters are variables that can be dynamically bound.
    - make-parameter: (make-parameter v [guard name])
        Returns a new parameter procedure. The value of the parameter is initialized to v in all threads.
    - (parameterize ([parameter-expr value-expr] ...) body ...)
        The result of a parameterize expression is the result of the last body.
        This is preferable to set! for several reasons (tail calls, threads, exceptions).
    > (define color (make-parameter "Blue"))
      (define (best-color) (display (color)))
      (best-color)
      (parameterize ([color "red"])
       (best-color))
      (best-color)

13.5 Writing
    There are at least three ways to output data (to the console).
    - `display` removes all quotation marks and string delimiters
    - `print` does not remove any quotation or string delimiters
    - `write` remoes the outermost quotation mark if any
    > (displayln '(a "azer" 3))
      (println '(a "azer" 3))
      (writeln '(a "azer" 3))
      (a azer 3)
      '(a "azer" 3)
      (a "azer" 3)

|#