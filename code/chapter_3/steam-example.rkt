#lang racket


(define (memo-proc proc)
  (let ([already-run? #f]
        [result #f])
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? #t)
            result)
          result))))

; delay and force are special forms!

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-null? stream) (null? stream))
(define the-empty-stream '())

(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))]))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (add1 n))))

(define neturals (integers-starting-from 0))
(define integers (integers-starting-from 1))

(define (stream-map proc . argstreams) ;argstreams是若干个参数,每个参数都是一个stream
  ;stream-map 选出每个流的第一个元素,以他们作为参数调 用proc形成一个结果,再以每个流的第二个元素作为参数,
  ; 调用proc形成一个结果 ...然后把所有结果弄成一个流。
  (if (stream-null? argstreams)
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map (cons proc (map stream-cdr argstreams))))))

(define (stream-filter proc s)
  (if (stream-null? s)
      the-empty-stream
      (let* ([a (stream-car s)])
        (if (proc a)
            (cons-stream a (stream-filter proc (stream-cdr s)))
            (stream-filter proc (stream-cdr s))))))
             
(define (scale-stream stream factor)
  (stream-map (curry * factor) stream))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (sub1 n))))

(define (display-stream s n) ;display first n items of s
  (if (= n 0)
      (displayln "")
      (begin (display (stream-car s)) (display " ") (display-stream (stream-cdr s) (- n 1)))))

(stream-car integers)
(stream-car (stream-cdr integers))
(stream-car (stream-map + integers integers))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))
(set! integers (cons-stream 1 (add-streams ones integers)))
(stream-car integers)
(stream-car (stream-cdr integers))

(define double (cons-stream 1 (scale-stream double 2)))
(stream-car double)
(stream-car (stream-cdr double))
(stream-car (stream-cdr (stream-cdr double)))

(define (partial-sums s)
  (define tmps
    (cons-stream (stream-car s) (add-streams tmps (stream-cdr s))))
  tmps)

(define (stream-append . ss)
  (if (null? ss)
      the-empty-stream
      (let [(s (first ss))]
        (if (stream-null? s)
            (apply stream-append (rest ss))
            (cons-stream (stream-car s)
                         (apply stream-append (cons (stream-cdr s) (rest ss))))))))

(displayln "=========== fib ==========")

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams fibs (stream-cdr fibs)))))
(for ([i (in-range 10)])
  (display (stream-ref fibs i))
  (display ","))
(newline)

(displayln "=========== prime ==========")

(define (divisible? x y) (= (remainder  x y) 0))

(define (sieve s)
  (cons-stream
   (stream-car s)
   (sieve (stream-filter
           (compose1 not (curryr divisible? (stream-car s)))
           (stream-cdr s)))))

(define primes (sieve (stream-cdr integers)))

(for ([i (in-range 10)])
  (display (stream-ref primes i))
  (display ","))
(newline)

(define (prime? n)
  (define (iter ps)
    (cond
      [(> (sqr (stream-car ps)) n) #t]
      [(divisible? n (stream-car ps)) #f]
      [else (iter (stream-cdr ps))]))
  (iter  primes))

(set! primes
      (cons-stream
            2
            (stream-filter prime? (integers-starting-from 3))))

(for ([i (in-range 10)])
  (display (stream-ref primes i))
  (display ","))
(newline)

(displayln "=========== sqrt ==========")

(define (average x y) (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))
 
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (curryr sqrt-improve x)
                             guesses)))
  guesses)

(define calc-sqrt-2 (sqrt-stream 2))

(for ([i (in-range 10)])
  (displayln (stream-ref calc-sqrt-2 i)))
(newline)

(displayln "=========== Pi ==========")

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(displayln (stream-ref  pi-stream 100))
(displayln (stream-ref  pi-stream 1000))

(define (euler-transform s)
  (let ([s0 (stream-ref s 0)] ; Sn-1
        [s1 (stream-ref s 1)] ; Sn
        [s2 (stream-ref s 2)]) ; Sn+1
        (cons-stream (- s2 (/ (sqr (- s2 s1))
                              (+ s0 (* -2 s1) s2)))
              (euler-transform (stream-cdr s)))))

(displayln (stream-ref  (euler-transform pi-stream) 100))

(define (make-tableau transform s) ; 返回值是个每个元素都是流的流 
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define acce-pi-stream (accelerated-sequence euler-transform pi-stream))
(for ([i (in-range 10)])
  (display i)
  (display " : ")
  (display (stream-ref acce-pi-stream i))
  (newline))

(displayln "=========== pair ==========")

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   `(,(stream-car s) ,(stream-car t))
   (interleave
    (stream-map (curry list (stream-car s))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define int-pairs (pairs neturals neturals))

(displayln "===========")

(for ([i (in-range 10)])
  (display i)
  (display " : ")
  (display (stream-ref int-pairs i))
  (newline))


(define add-prime (stream-filter
                   (lambda (pair) (prime? (+ (car pair) (cadr pair))))
                   int-pairs))

(displayln "===========")

(for ([i (in-range 30)])
  (display i)
  (display " : ")
  (display (stream-ref add-prime i))
  (newline))


(displayln "=========== integral ==========")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define int (integral integers 0 1))
(for ([i (in-range 10)])
  (display i)
  (display " : ")
  (display (stream-ref int i))
  (newline))


(set! integral (lambda (delayed-integrand initial-value dt)
   (define int
     (cons-stream initial-value
                (let ([integrand (force delayed-integrand)])
                  (add-streams (scale-stream integrand dt)
                               int))))
                 int))

 (define (solve f y0 dt)
   (define y (integral (delay dy) y0 dt))
   (define dy (stream-map f y))
   y)

 (displayln (stream-ref (solve (lambda (y) y) 1 0.001) 1000))


(displayln "=========== modulization ==========")

(define random-init 1)
(define (random-update x) (remainder (+ (* 13 x) 5) 24))

(define random-numbers
  (cons-stream random-init
               (stream-map random-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))



(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream (/ passed (+ passed failed))
                 (monte-carlo
                  (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi (stream-map (lambda (p) (sqrt (/ 6 p)))
                       (monte-carlo cesaro-stream 0 0)))

(display-stream pi 100)
