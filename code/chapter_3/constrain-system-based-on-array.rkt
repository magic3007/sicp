#lang racket


(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (not (has-value? me))
          (void)
          (begin
            (set! informant false)
            (for-each-except retractor
                             inform-about-no-value
                             constraints))))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints))
          (void))
      (if (has-value? me)
          (inform-about-value new-constraint)
          (void))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNECTOR"
                         request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (probe name connector)
  (define (print-probe value)
    (display "Probe: ") (display name)
    (display " = ") (displayln value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (void))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond [(and (has-value? a1) (has-value? a2))
           (set-value! sum (+ (get-value a1) (get-value a2)) me)]
          [(and (has-value? a1) (has-value? sum))
           (set-value! a2 (- (get-value sum) (get-value a1)) me)]
          [(and (has-value? a2) (has-value? sum))
           (set-value! a1 (- (get-value sum) (get-value a2)) me)]))
  
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: ADDER" request))))

  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (adder-list a1 a2 sum)
  (for-each adder a1 a2 sum))

(define (get-value-list lst)
  (map get-value lst))

(define (set-value-list! lst ref val informant)
  (set-value! (list-ref lst ref) val informant))

(define (probe-list list-name lst)
  (define (probe-helper lst ref)
    (if (null? lst)
        (void)
        (begin
          (probe (string-append list-name
                                (number->string ref))
                 (car lst))
          (probe-helper (cdr lst) (+ ref 1)))))
  (probe-helper lst 0))

(define (init-connector n)
  (if (= n 0)
      '()
      (cons (make-connector)
            (init-connector (- n 1)))))
             
(define (solve)
  (let ((n (read)))
    (let ((a (init-connector n))
          (b (init-connector n))
          (s (init-connector n)))
      (probe-list "a" a)
      (probe-list "b" b)
      (probe-list "s" s)
      (adder-list a b s)
      (operate a b s)
      (displayln (get-value-list a))
      (displayln (get-value-list b))
      (displayln (get-value-list s)))))

(define (operate a b s)
  (let ((lst (read))
        (ref (read))
        (val (read)))
    (if (eq? lst eof)
        (void)
        (begin
          (cond
            ((eq? lst 'a) (set-value-list! a ref val 'user))
            ((eq? lst 'b) (set-value-list! b ref val 'user))
            ((eq? lst 's) (set-value-list! s ref val 'user)))
          (operate a b s)))))

(solve)