****1: SICP课本练习 3.30
by 1900012959(lny)
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



(define (call-each procedures) 
  (if (null? procedures)
      (void)
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0) 
        (action-procedures '())) 
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
               (begin (set! signal-value new-value)
                      (call-each action-procedures)) 
               (void))) ;ori: done-of-set-my-signal
    (define (accept-action-procedure! proc) 
      (set! action-procedures (cons proc action-procedures))
      (proc)) 
    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "unknown operation -- wire" m))))
    dispatch))
      
(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


(define (make-time-segment time queue) 
  (cons time queue))

(define (segment-time s) (car s)) 
(define (segment-que s) (cdr s))



(define (make-agenda) (list 0))  
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda)) 

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))


(define (add-to-agenda! time action agenda) 
  (define (belongs-before? segments)  
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action) 
    (let ((q (make-que)))
      (insert-que! q action)
      (make-time-segment time q)))
  
  (define (add-to-segments! segments)  ;add action to segments according to time
  
    (if (= (segment-time (car segments)) time)
        (insert-que! (segment-que (car segments))
                     action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments (cons (make-new-time-segment time action)
                                       (cdr segments)))  
              (add-to-segments! rest)))))
  
  (let ((segments-a (segments agenda)))
    (if (belongs-before? segments-a)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments-a))
        (add-to-segments! segments-a))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-que (first-segment agenda))))
    (delete-que! q)
    (if (empty-que? q)
        (set-segments! agenda (rest-segments agenda))
        (void)))) ;ori: 'ok-remove-first-agenda-item!

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is emptyP")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg)) ;set the time in agenda to the time of first item when accessing the first item
        (front-que (segment-que first-seg)))))


;the-agenda is the global variable
(define the-agenda (make-agenda))

(define (propagate)  
  (if (empty-agenda? the-agenda)
      (void) ;ori : 'done-agenda-is-empty
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))


;every thing is happened in the-agentda ,so after-delay is handling the-agenda -- just add action to the-agenda
;then propagate excutes all actions in the-agenda
(define (after-delay delay action) 
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))





(define inverter-delay 2) 
(define and-gate-delay 3) 
(define or-gate-delay 5) 




(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  new-value = ")
                 (display (get-signal wire))
                 (newline))))



(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))


(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  (void))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "invalid signal" s))))
(define (logical-and a1 a2)
  (cond ((or (= a1 0) (= a2 0)) 0)
        ((and (= a1 1) (= a2 1)) 1)
        (else (error "invalid signal" a1 " " a2))))

(define (logical-or a1 a2)
  (cond ((or (= a1 1) (= a2 1)) 1)
        ((and (= a1 0) (= a2 0)) 0)
        (else (error "invalid signal" a1 " " a2))))



(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  (void)) ;ori: 'ok-and-gate


(define (or-gate a1 a2 output)  
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  (void)) ;ori: 'ok-orgate

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    (void))) ;ori 'ok-half-adder

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    (void))) ;ori 'ok-full-adder


(define (ripple-carry-adder wire-lstA wire-lstB wire-lstS wireCN)
(define (f1 a b l s c)
    (if (null? a) (void)
        (let [(t (if (null? (cdr a)) c (make-wire)))]
          (full-adder (car a) (car b) l (car s) t)
          (f1 (cdr a) (cdr b) t (cdr s) c)
          )
        )
    )
  (f1 wire-lstA wire-lstB (make-wire) wire-lstS wireCN)
)
(define (make-wire-list-N n) ;make a list of n wires
  (if (= n 0)
      '()
      (cons (make-wire) (make-wire-list-N (- n 1)))))

(define (set-values ws vs) ;set the value of wires in ws according to list vs
  (if (null? ws)
      (void)
      (begin (set-signal! (car ws) (car vs)) (set-values (cdr ws) (cdr vs)))))

(define (get-values ws) ;return the list of values of wires in list ws
  (if (null? ws)
      '()
      (cons (get-signal (car ws)) (get-values (cdr ws)))))

(define n (read))
(define A (make-wire-list-N n))
(define B (make-wire-list-N n))
(define S (make-wire-list-N n))
(define C (make-wire))
(ripple-carry-adder A B S C)

(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (set-values A (list->mlist a))
               (set-values B (list->mlist b))
               (propagate)
               (displayln (get-values S))
               (displayln (get-signal C))
               (myloop)))))

(myloop)

by 1800012901
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



(define (call-each procedures) 
  (if (null? procedures)
      (void)
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0) 
        (action-procedures '())) 
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
               (begin (set! signal-value new-value)
                      (call-each action-procedures)) 
               (void))) ;ori: done-of-set-my-signal
    (define (accept-action-procedure! proc) 
      (set! action-procedures (cons proc action-procedures))
      (proc)) 
    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "unknown operation -- wire" m))))
    dispatch))
      
(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


(define (make-time-segment time queue) 
  (cons time queue))

(define (segment-time s) (car s)) 
(define (segment-que s) (cdr s))



(define (make-agenda) (list 0))  
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda)) 

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))


(define (add-to-agenda! time action agenda) 
  (define (belongs-before? segments)  
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action) 
    (let ((q (make-que)))
      (insert-que! q action)
      (make-time-segment time q)))
  
  (define (add-to-segments! segments)  ;add action to segments according to time
  
    (if (= (segment-time (car segments)) time)
        (insert-que! (segment-que (car segments))
                     action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments (cons (make-new-time-segment time action)
                                       (cdr segments)))  
              (add-to-segments! rest)))))
  
  (let ((segments-a (segments agenda)))
    (if (belongs-before? segments-a)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments-a))
        (add-to-segments! segments-a))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-que (first-segment agenda))))
    (delete-que! q)
    (if (empty-que? q)
        (set-segments! agenda (rest-segments agenda))
        (void)))) ;ori: 'ok-remove-first-agenda-item!

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is emptyP")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg)) ;set the time in agenda to the time of first item when accessing the first item
        (front-que (segment-que first-seg)))))


;the-agenda is the global variable
(define the-agenda (make-agenda))

(define (propagate)  
  (if (empty-agenda? the-agenda)
      (void) ;ori : 'done-agenda-is-empty
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))


;every thing is happened in the-agentda ,so after-delay is handling the-agenda -- just add action to the-agenda
;then propagate excutes all actions in the-agenda
(define (after-delay delay action) 
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))





(define inverter-delay 2) 
(define and-gate-delay 3) 
(define or-gate-delay 5) 




(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  new-value = ")
                 (display (get-signal wire))
                 (newline))))



(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))


(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  (void))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "invalid signal" s))))
(define (logical-and a1 a2)
  (cond ((or (= a1 0) (= a2 0)) 0)
        ((and (= a1 1) (= a2 1)) 1)
        (else (error "invalid signal" a1 " " a2))))

(define (logical-or a1 a2)
  (cond ((or (= a1 1) (= a2 1)) 1)
        ((and (= a1 0) (= a2 0)) 0)
        (else (error "invalid signal" a1 " " a2))))



(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  (void)) ;ori: 'ok-and-gate


(define (or-gate a1 a2 output)  
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  (void)) ;ori: 'ok-orgate

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    (void))) ;ori 'ok-half-adder

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    (void))) ;ori 'ok-full-adder


(define (ripple-carry-adder wire-lstA wire-lstB wire-lstS wireCN)
	(define (work A B S C)
		(let ((c (make-wire)))
			(if (null? (cdr A))
				(full-adder (car A) (car B) C (car S) wireCN)
				(begin
					(full-adder (car A) (car B) C (car S) c)
					(work (cdr A) (cdr B) (cdr S) c)
				)
			)
		)
	)
	(let ((c0 (make-wire)))
		(work wire-lstA wire-lstB wire-lstS c0)
	)
)
(define (make-wire-list-N n) ;make a list of n wires
  (if (= n 0)
      '()
      (cons (make-wire) (make-wire-list-N (- n 1)))))

(define (set-values ws vs) ;set the value of wires in ws according to list vs
  (if (null? ws)
      (void)
      (begin (set-signal! (car ws) (car vs)) (set-values (cdr ws) (cdr vs)))))

(define (get-values ws) ;return the list of values of wires in list ws
  (if (null? ws)
      '()
      (cons (get-signal (car ws)) (get-values (cdr ws)))))

(define n (read))
(define A (make-wire-list-N n))
(define B (make-wire-list-N n))
(define S (make-wire-list-N n))
(define C (make-wire))
(ripple-carry-adder A B S C)

(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (set-values A (list->mlist a))
               (set-values B (list->mlist b))
               (propagate)
               (displayln (get-values S))
               (displayln (get-signal C))
               (myloop)))))

(myloop)

by 1800012802
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



(define (call-each procedures) 
  (if (null? procedures)
      (void)
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0) 
        (action-procedures '())) 
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
               (begin (set! signal-value new-value)
                      (call-each action-procedures)) 
               (void))) ;ori: done-of-set-my-signal
    (define (accept-action-procedure! proc) 
      (set! action-procedures (cons proc action-procedures))
      (proc)) 
    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "unknown operation -- wire" m))))
    dispatch))
      
(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


(define (make-time-segment time queue) 
  (cons time queue))

(define (segment-time s) (car s)) 
(define (segment-que s) (cdr s))



(define (make-agenda) (list 0))  
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda)) 

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))


(define (add-to-agenda! time action agenda) 
  (define (belongs-before? segments)  
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action) 
    (let ((q (make-que)))
      (insert-que! q action)
      (make-time-segment time q)))
  
  (define (add-to-segments! segments)  ;add action to segments according to time
  
    (if (= (segment-time (car segments)) time)
        (insert-que! (segment-que (car segments))
                     action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments (cons (make-new-time-segment time action)
                                       (cdr segments)))  
              (add-to-segments! rest)))))
  
  (let ((segments-a (segments agenda)))
    (if (belongs-before? segments-a)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments-a))
        (add-to-segments! segments-a))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-que (first-segment agenda))))
    (delete-que! q)
    (if (empty-que? q)
        (set-segments! agenda (rest-segments agenda))
        (void)))) ;ori: 'ok-remove-first-agenda-item!

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is emptyP")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg)) ;set the time in agenda to the time of first item when accessing the first item
        (front-que (segment-que first-seg)))))


;the-agenda is the global variable
(define the-agenda (make-agenda))

(define (propagate)  
  (if (empty-agenda? the-agenda)
      (void) ;ori : 'done-agenda-is-empty
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))


;every thing is happened in the-agentda ,so after-delay is handling the-agenda -- just add action to the-agenda
;then propagate excutes all actions in the-agenda
(define (after-delay delay action) 
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))





(define inverter-delay 2) 
(define and-gate-delay 3) 
(define or-gate-delay 5) 




(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  new-value = ")
                 (display (get-signal wire))
                 (newline))))



(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))


(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  (void))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "invalid signal" s))))
(define (logical-and a1 a2)
  (cond ((or (= a1 0) (= a2 0)) 0)
        ((and (= a1 1) (= a2 1)) 1)
        (else (error "invalid signal" a1 " " a2))))

(define (logical-or a1 a2)
  (cond ((or (= a1 1) (= a2 1)) 1)
        ((and (= a1 0) (= a2 0)) 0)
        (else (error "invalid signal" a1 " " a2))))



(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  (void)) ;ori: 'ok-and-gate


(define (or-gate a1 a2 output)  
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  (void)) ;ori: 'ok-orgate

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    (void))) ;ori 'ok-half-adder

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    (void))) ;ori 'ok-full-adder


(define (ripple-carry-adder wire-lstA wire-lstB wire-lstS wireCN)
  (define (Rca wlA wlB c wlS out)
    (let((T (make-wire)))
      (if(null? wlA)(or-gate c c out)
         (begin
           (full-adder (car wlA) (car wlB) c (car wlS) T)
           (Rca (cdr wlA) (cdr wlB) T (cdr wlS) out)
           (void)))))
  (let ((t (make-wire)))(Rca wire-lstA wire-lstB t wire-lstS wireCN)))

(define (make-wire-list-N n) ;make a list of n wires
  (if (= n 0)
      '()
      (cons (make-wire) (make-wire-list-N (- n 1)))))

(define (set-values ws vs) ;set the value of wires in ws according to list vs
  (if (null? ws)
      (void)
      (begin (set-signal! (car ws) (car vs)) (set-values (cdr ws) (cdr vs)))))

(define (get-values ws) ;return the list of values of wires in list ws
  (if (null? ws)
      '()
      (cons (get-signal (car ws)) (get-values (cdr ws)))))

(define n (read))
(define A (make-wire-list-N n))
(define B (make-wire-list-N n))
(define S (make-wire-list-N n))
(define C (make-wire))
(ripple-carry-adder A B S C)

(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (set-values A (list->mlist a))
               (set-values B (list->mlist b))
               (propagate)
               (displayln (get-values S))
               (displayln (get-signal C))
               (myloop)))))

(myloop)

by 1800012789
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



(define (call-each procedures) 
  (if (null? procedures)
      (void)
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0) 
        (action-procedures '())) 
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
               (begin (set! signal-value new-value)
                      (call-each action-procedures)) 
               (void))) ;ori: done-of-set-my-signal
    (define (accept-action-procedure! proc) 
      (set! action-procedures (cons proc action-procedures))
      (proc)) 
    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "unknown operation -- wire" m))))
    dispatch))
      
(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


(define (make-time-segment time queue) 
  (cons time queue))

(define (segment-time s) (car s)) 
(define (segment-que s) (cdr s))



(define (make-agenda) (list 0))  
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda)) 

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))


(define (add-to-agenda! time action agenda) 
  (define (belongs-before? segments)  
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action) 
    (let ((q (make-que)))
      (insert-que! q action)
      (make-time-segment time q)))
  
  (define (add-to-segments! segments)  ;add action to segments according to time
  
    (if (= (segment-time (car segments)) time)
        (insert-que! (segment-que (car segments))
                     action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments (cons (make-new-time-segment time action)
                                       (cdr segments)))  
              (add-to-segments! rest)))))
  
  (let ((segments-a (segments agenda)))
    (if (belongs-before? segments-a)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments-a))
        (add-to-segments! segments-a))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-que (first-segment agenda))))
    (delete-que! q)
    (if (empty-que? q)
        (set-segments! agenda (rest-segments agenda))
        (void)))) ;ori: 'ok-remove-first-agenda-item!

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is emptyP")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg)) ;set the time in agenda to the time of first item when accessing the first item
        (front-que (segment-que first-seg)))))


;the-agenda is the global variable
(define the-agenda (make-agenda))

(define (propagate)  
  (if (empty-agenda? the-agenda)
      (void) ;ori : 'done-agenda-is-empty
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))


;every thing is happened in the-agentda ,so after-delay is handling the-agenda -- just add action to the-agenda
;then propagate excutes all actions in the-agenda
(define (after-delay delay action) 
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))





(define inverter-delay 2) 
(define and-gate-delay 3) 
(define or-gate-delay 5) 




(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  new-value = ")
                 (display (get-signal wire))
                 (newline))))



(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))


(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  (void))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "invalid signal" s))))
(define (logical-and a1 a2)
  (cond ((or (= a1 0) (= a2 0)) 0)
        ((and (= a1 1) (= a2 1)) 1)
        (else (error "invalid signal" a1 " " a2))))

(define (logical-or a1 a2)
  (cond ((or (= a1 1) (= a2 1)) 1)
        ((and (= a1 0) (= a2 0)) 0)
        (else (error "invalid signal" a1 " " a2))))



(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  (void)) ;ori: 'ok-and-gate


(define (or-gate a1 a2 output)  
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  (void)) ;ori: 'ok-orgate

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    (void))) ;ori 'ok-half-adder

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    (void))) ;ori 'ok-full-adder


(define (ripple-carry-adder wire-lstA wire-lstB wire-lstS wireCN)
  (define Cn (make-wire-list-N n))
  (define (helper An Bn Cn Sn C)
    (if (= 1 (mlength An))
        (full-adder (car An) (car Bn) (car Cn) (car Sn) C)
        (begin
          (full-adder (car An) (car Bn) (car Cn) (car Sn) (cadr Cn))
          (helper (cdr An) (cdr Bn) (cdr Cn) (cdr Sn) C))))
  (helper wire-lstA wire-lstB Cn wire-lstS wireCN))
(define (make-wire-list-N n) ;make a list of n wires
  (if (= n 0)
      '()
      (cons (make-wire) (make-wire-list-N (- n 1)))))

(define (set-values ws vs) ;set the value of wires in ws according to list vs
  (if (null? ws)
      (void)
      (begin (set-signal! (car ws) (car vs)) (set-values (cdr ws) (cdr vs)))))

(define (get-values ws) ;return the list of values of wires in list ws
  (if (null? ws)
      '()
      (cons (get-signal (car ws)) (get-values (cdr ws)))))

(define n (read))
(define A (make-wire-list-N n))
(define B (make-wire-list-N n))
(define S (make-wire-list-N n))
(define C (make-wire))
(ripple-carry-adder A B S C)

(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (set-values A (list->mlist a))
               (set-values B (list->mlist b))
               (propagate)
               (displayln (get-values S))
               (displayln (get-signal C))
               (myloop)))))

(myloop)

****2: 基于数组的约束系统
by 1900012959(lny)
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
(define (adder-list a b s)
  (define (adder x y z)
    (define (newval)
      (cond [(and (has-value? x) (has-value? y))
             (set-value! z (+ (get-value x) (get-value y)) me)]
            [(and (has-value? x) (has-value? z))
             (set-value! y (- (get-value z) (get-value x)) me)]
            [(and (has-value? y) (has-value? z))
             (set-value! x (- (get-value z) (get-value y)) me)]
            )
      )
    (define (me req)
      (if (eq? req 'I-have-a-value) (newval) (void))
      )
    (connect x me)(connect y me)(connect z me)
    me
    )
  (define (f1 x y z) (if (null? x) (void) (begin (adder (car x) (car y) (car z)) (f1 (cdr x) (cdr y) (cdr z)))))
  (f1 a b s)
  )
(define (get-value-list x) (map get-value x))
(define (set-value-list! x y z i) (if (> y 0) (set-value-list! (cdr x) (- y 1) z i) (set-value! (car x) z i)))
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

by 1700012751
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

by 1700012846(yeongmin)
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
(define (adder-list a b s)
  (define (adder a b s)
    (define (process-new-value)
      (cond ((and (has-value? a) (has-value? b))
             (set-value! s
                         (+ (get-value a) (get-value b))
                         me))
            ((and (has-value? a) (has-value? s))
             (set-value! b
                         (- (get-value s) (get-value a))
                         me))
            ((and (has-value? b) (has-value? s))
             (set-value! a
                         (- (get-value s) (get-value b))
                         me))))
    (define (process-forget-value)
      (map (lambda (t) (forget-value! t me))
           (list a b s))
      (process-new-value))
    (define (me request)
      (case request
        ((I-have-a-value) (process-new-value))
        ((I-lost-my-value) (process-forget-value))
        (else (error "Unknown request -- ADDER" request))))
    (map (lambda (t) (connect t me))
         (list a b s))
    me)
  (map adder a b s))
(define (get-value-list l) (map get-value l))
(define (set-value-list! l pos val informant) (set-value! (list-ref l pos) val informant))
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

by 1800012789
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
(define (adder a b s)
  (define (process-new-value)
    (cond
      ((and (has-value? a) (has-value? b))
       (set-value! s (+ (get-value a) (get-value b)) me))
      ((and (has-value? a) (has-value? s))
       (set-value! b (- (get-value s) (get-value a)) me))
      ((and (has-value? b) (has-value? s))
       (set-value! a (- (get-value s) (get-value b)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (forget-value! s me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))
  (connect a me)
  (connect b me)
  (connect s me)
  me)

(define (get-value-list a)
  (if (empty? a)
      '()
      (cons (get-value (car a)) (get-value-list (cdr a)))))

(define (set-value-list! lst ref val informant)
  (set-value! (list-ref lst ref) val informant))

(define (adder-list a b s)
  (if (empty? a)
      (void)
      (begin
        (adder (car a) (car b) (car s))
        (adder-list (cdr a) (cdr b) (cdr s)))))

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

