#lang racket

; http://lisp.test.openjudge.org/2020hw12/002/

(require scheme/mpair)

;==================================================

(define (make-frame syms vals)
  (mmap mcons (list->mlist syms) (list->mlist vals)))

(define (frame-look-up frame sym)
  (let ([rv (massoc sym frame)])
    (if rv
        (values (mcdr rv) #t)
        (values (void) #f))))

(define (frame-set! frame sym val)
  (let ([rv (massoc sym frame)])
    (if rv (begin (set-mcdr! rv val) #t) #f)))

;==================================================

(struct EnvType ([frame #:mutable] ex-env))

(define (env-look-up env sym)
  (if (null? env) (error "unbounded symbol: " sym)
      (let-values ([(rv found?) (frame-look-up (EnvType-frame env) sym)])
        (if found? rv (env-look-up (EnvType-ex-env env) sym)))))

(define (env-set! env sym val)
  (if (null? env) (error "unbounded symbol: " sym)
      (let ([rv (frame-set! (EnvType-frame env) sym val)])
        (if rv rv (env-set! (EnvType-ex-env env) sym val)))))

(define (env-define env sym val)
  (set-EnvType-frame! env (mcons (mcons sym val) (EnvType-frame env))))

;===================================================

(define (syms->frame syms)
  (if (null? syms) '()
      (mmap (lambda (sym)
              (if (pair? sym)
                  (mcons (first sym) (second sym))
                  (mcons sym (eval sym (make-base-namespace)))))
            (list->mlist syms))))

(define primitive-frame
  (syms->frame
   `(+ - * / > < = >= <= quotient remainder cons car cdr cadr
       list not length append sqrt displayln void display newline
       number? symbol? null? pair? eq? equal? (false #f) (true #t))))

(define primitive-env (EnvType primitive-frame '()))

;===================================================

; return a dispatching function whose result is a list of all the results in |lst|
(define (serialize lst)
  (define (foo lst)
    (if (null? lst)
        (lambda (env scd fail) (scd '() fail))
        (lambda (env scd fail)
          ((first lst) env
                       (lambda (rv fail2)
                         ((foo (rest lst)) env
                                            (lambda (rs fail3)
                                              (scd (cons rv rs) fail3))
                                            fail2))
                       fail))))
  (foo lst))

;===================================================

(struct ProcedureType (syms fbody env))

(define (run-fproc fproc args env scd fail)
  (let* ([fargs (map analyze args)]
         [fe (serialize fargs)])
    (fproc env
           (lambda (proc fail2)
             (fe env
                 (lambda (vals fail3)
                   (match proc
                     [(ProcedureType syms fbody proc-env)
                      (fbody (EnvType (make-frame syms vals) proc-env)
                             scd fail3)]
                     [primitive-proc
                      (scd (apply primitive-proc vals) fail3)]))
                 fail2))
           fail)))

;===================================================

(define (analyze-begin bodys)
  (let* ([fbodys (map analyze bodys)]
         [fe (serialize fbodys)])
    (lambda (env scd fail)
      (fe env (lambda (rv fail2)
                (scd (last rv) fail2))
          fail))))

(define (analyze-apply proc args)
  (let ([fproc (analyze proc)])
    (lambda (env scd fail)
      (run-fproc fproc args env scd fail))))

(define (analyze-lambda syms bodys)
  (let ([fbody (analyze-begin bodys)])
    (lambda (env scd fail)
      (scd (ProcedureType syms fbody env) fail))))

(define (analyze-amb exps)
  (let ([fexps (map analyze exps)])
    (lambda (env scd fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((first choices) env scd (lambda () (try-next (rest choices))))))
      (try-next fexps))))
    
(define (analyze-set! sym exp)
  (let ([fexp (analyze exp)])
    (lambda (env scd fail)
      (fexp env
            (lambda (val fail2)
              (let* ([old-value (env-look-up env sym)])
                (env-set! env sym val)
                (scd (void)
                     (lambda () (env-set! env sym old-value) (fail2)))))
            fail))))

(define (analyze-define sym fexp)
  (lambda (env scd fail)
    (fexp env (lambda (rv fail2)
               (env-define env sym rv)
               (scd (void) fail2))
         fail)))

(define (analyze-if . args)
  (match (map analyze args)
    [`(,fpred ,fconseq ,falter)
     (lambda (env scd fail)
       (fpred env
              (lambda (rv fail2)
                (if rv
                    (fconseq env scd fail2)
                    (falter env scd fail2)))
              fail))]
    [_ (error "analyze-if error")]))

(define (analyze-let pairs bodys)
  (let* ([syms (map first pairs)]
         [args (map second pairs)]
         [fproc (analyze-lambda syms bodys)])
    (lambda (env scd fail)
      (run-fproc fproc args env scd fail))))

(define (analyze-cond pairs)
  (define (foo pa)
    (match pa
      [`(else . ,bodys) `(else  ,(analyze-begin bodys))]
      [`(,pred . ,bodys) #:when (not (null? bodys)) `(,(analyze pred)  ,(analyze-begin bodys))]
      [`(,pred) (analyze pred)]
      [_ (error "Error: analyze-cond")]))
  (let ([fpairs (map foo pairs)])
      (define (scan e)
        (lambda (env scd fail)
          (if (null? e) (scd (void) fail)
              (match (first e)
                [`(else ,fbody) (fbody env scd fail)]
                [`(,fpred ,fbody)
                 (fpred env (lambda (rv fail2)
                              (if rv (fbody env scd fail2)
                                  ((scan (rest e)) env scd fail2)))
                        fail)]
                [fpred
                 (fpred env (lambda (rv fail2)
                              (if rv (scd rv fail2)
                                  ((scan (rest e)) env scd fail2)))
                              fail)]))))
    (scan fpairs)))

(define (analyze-and exps)
  (let ([fexps (map analyze exps)])
    (define (foo lst)
      (if (null? (rest lst))
          (first lst)
          (lambda (env scd fail)
            ((first lst) env
                         (lambda (rv fail2)
                           (if (not rv) rv
                               ((foo (rest  lst)) env scd fail)))
                         fail))))
    (foo fexps)))

(define (analyze-or exps)
  (let ([fexps (map analyze exps)])
    (define (foo lst)
      (if (null? (rest lst))
          (first lst)
          (lambda (env scd fail)
            ((first lst) env
                         (lambda (rv fail2)
                           (if rv rv
                               ((foo (rest  lst)) env scd fail)))
                         fail))))
    (foo fexps)))

(define (analyze-all-answer exp)
  (let ([fexp (analyze exp)])
    (lambda (env scd fail)
      (fexp env
            (lambda (rv fail2)
              (displayln rv)
              (fail2))
            (lambda () (scd (void) fail))))))

(define (analyze-if-fail conseq alter)
  (let ([fconseq (analyze conseq)]
        [falter (analyze alter)])
    (lambda (env scd fail)
      (fconseq env
               (lambda (rv fail2)
                 (scd rv fail))
               (lambda() (falter env scd fail))))))

(define (self-evaluating? e)
  (or (number? e) (string? e) (boolean? e)))

(define (analyze e)
  (match e
    [e #:when (self-evaluating? e) (lambda (env scd fail) (scd e fail))]
    [`',x (lambda (env scd fail) (scd x fail))]
    [`(begin . ,bodys) (analyze-begin bodys)]
    [`(apply ,proc . ,args) (analyze-apply proc args)] 
    [`(lambda ,syms . ,bodys) (analyze-lambda syms bodys)]
    [`(set! ,sym ,exp) (analyze-set! sym exp)]
    [`(define (,proc . ,syms) . ,bodys) (analyze-define proc (analyze-lambda syms bodys))]
    [`(define ,sym ,exp) (analyze-define sym (analyze exp))]
    [`(let ,pairs . ,bodys) (analyze-let pairs bodys)]
    [`(if ,pred ,conseq ,alter) (analyze-if pred conseq alter)]
    [`(if-fail ,conseq ,alter) (analyze-if-fail conseq alter)]
    [`(cond . ,pairs) (analyze-cond pairs)]
    [`(and . ,exps) (analyze-and exps)]
    [`(or . ,exps) (analyze-or exps)]
    [`(amb . ,exps) (analyze-amb exps)]
    [`(all-answer ,exp) (analyze-all-answer exp)]
    [`(,proc . ,args) (analyze-apply proc args)]
    [x (lambda (env scd fail) (scd (env-look-up env x) fail))]))

(define glb-scd  (lambda (x fail) x))
(define glb-fail (lambda () (displayln "There are no more answers.")))

(define (amb-eval scd fail e)
  ((analyze e) primitive-env scd fail))

(amb-eval glb-scd glb-fail `(define (require p)
             (if (not p) (amb) (void))))

; ======================================================================

(define (check e ans)
  (let ([result (amb-eval glb-scd glb-fail e)])
    (if (equal? result ans) (void)
        (begin (display ans)
               (display " expected, but ")
               (display result)
               (displayln " found")
               (error "assertion failed")))))

(check '(define (x) 1 2) (void))
(check '(x) 2)
(check '(define x 2) (void))
(check 'x 2)
(check '(set! x 3) (void))
(check 'x 3)
(check '(cond [true 123 321] [else 'what 'ok]) 321)
(check '(cond [false 222] [else 'what 'ok]) 'ok)
(check '(begin (+ 1 2) 'hello) 'hello)
(check '((lambda (x y) x) 1 2) 1)
(check '(and (> 5 3) (> 6 2) (* 3 4)) 12)
(check '(if true 233 345) 233)
(check '(if true false 345) #f)
(check '(let ([x 1] [y 2]) x y) 2)
(check 'true #t)
(check '(and #f #t) #f)
(check '(or #f 555) 555)
(check '(define (rand-update x) (define a 97) (define b 103) (define m 127) (remainder (+ (* a x ) b) m)) (void))
(check '(define rand (let ((t 6)) (lambda () (set! t (rand-update t)) t))) (void))
(check '(rand-update 6) 50)
(check '(define haha (let ((t 6)) t)) (void))
(check 'haha 6)
(check '(list (rand) (rand) (rand) (rand)) '(50 0 103 61))

; ======================================================================

(define (driver-loop)
  (define (internal-loop try-again)
    (let ([input (read)])
      (when (eq? input eof) (exit))
      (if (eq? input 'try-again)
          (try-again)
          (amb-eval
           (lambda (rv next-alter)
             (when (not (eq? rv (void))) (displayln rv))
             (internal-loop next-alter))
           (lambda () (glb-fail) (driver-loop))
           input))))
  (internal-loop (lambda () (glb-fail) (driver-loop))))

(driver-loop)
