#lang racket

(define (for-each-except exception procedure list)
  (for-each procedure
   (filter-not (lambda (x) (eq? x exception)) list)))

; ============================================================
; helper

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


; ==========================================================
; connector

(define (make-connector);返回值是闭包 me
  (let ([value #f] ; 初始值
        [informant #f] ; 上一个命令其改变值的命令者
        [constraints '()]) ; 它加入的约束器列表
    (define (set-my-value newval setter);setter是命令者
      (cond [(not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter inform-about-value constraints)] ;依此通知列表中的约束器
            [(not (= value newval))
             (error "Contradiction" (list value newval))]
            [else 'ignored]))
    
    (define (forget-my-value retractor);retractor是命令者
      (if (eq? retractor informant);只有命令者才有权取消自己设置的值!!!!
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints (cons new-constraint constraints)) 'ok)
      (if (has-value? me)
          (inform-about-value new-constraint) 'ok)
      'done)
    
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR" request))))
    me))

; ==============================================================
; 约束器

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond [(and (has-value? a1) (has-value? a2))
           (set-value! sum (+ (get-value a1) (get-value a2)) me)]
          [(and (has-value? a1) (has-value? sum))
           (set-value! a2 (- (get-value sum) (get-value a1)) me)]
          [(and (has-value? a2) (has-value? sum))
           (set-value! a1 (- (get-value sum) (get-value a2)) me)]))
  
  (define (process-forget-value);有connector值被忘记时被调用,用来传播约束
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value)) ;为何忘记值也需要调用process-new-value? Hint: 考虑常量约束器constant
  
  (define (me request)
    (cond [(eq? request 'I-have-a-value) (process-new-value)]
          [(eq? request 'I-lost-my-value) (process-forget-value)]
          [else (error "Unknown request -- ADDER" request)]))
  
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)
; 每个connector有个与之相连的约束器列表。connector上的值变化时,会调用约束器列表中约束器
; 的 process-forget-value或 process-new-value

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond [(or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me)]
          [(and (has-value? m1) (has-value? m2))
           (set-value! product (* (get-value m1) (get-value m2)) me)]
          [(and (has-value? product) (has-value? m1))
           (set-value! m2 (/ (get-value product) (get-value m1)) me)]
          [(and (has-value? product) (has-value? m2))
           (set-value! m1 (/ (get-value product) (get-value m2)) me)]))
  
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  
  (define (me request)
    (cond [(eq? request 'I-have-a-value)
           (process-new-value)]
          [(eq? request 'I-lost-my-value)
           (process-forget-value)]
          [else (error "Unknown request: MULTIPLIER" request)]))
  
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me) ;会向 connector发送 'connect消息
  (set-value! connector value me);me命令connector将其值改为value
  me)

;=========================================================
; 探测器
(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ") (display name) (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

; ==============================================

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))


(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)


(set-value! C 25 'user)
(newline)
(forget-value! C 'user)
(newline)
(set-value! F 212 'user)

                                                  