#lang racket

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
       list not length append sqrt
       number? symbol? null? pair? eq? equal? (false #f) (true #t))))

(define primitive-env (EnvType primitive-frame '()))

;===================================================

(struct ProcedureType (syms fbody env))

(define (run-fproc fproc args env)
  (let ([vals (map (lambda (x) ((analyze x) env)) args)])
    (match (fproc env)
      [(ProcedureType syms fbody proc-env)
       (fbody (EnvType (make-frame syms vals) proc-env))]
      [primitive-proc (apply primitive-proc vals)])))
                                 
;===================================================

(define (analyze-begin bodys)
  (let ([fbodys (map analyze bodys)])
    (lambda (env)
      (for/last ([x fbodys]) (x env)))))

(define (analyze-apply proc args)
  (let ([fproc (analyze proc)])
    (lambda (env)
      (run-fproc fproc args env))))

(define (analyze-lambda syms bodys)
  [let ([fbody (analyze-begin bodys)])
    (lambda (env)
      (ProcedureType syms fbody env))])

(define (analyze-set! sym exp)
  (let ([fexp (analyze exp)])
    (lambda (env)
      (env-set! env sym (fexp env))
      (void))))

(define (analyze-define sym fexp)
  (lambda (env)
    (env-define env sym (fexp env))
    (void)))

(define (analyze-let pairs bodys)
  (let* ([syms (map first pairs)]
         [args (map second pairs)]
         [fproc (analyze-lambda syms bodys)])
    (lambda (env) (run-fproc fproc args env))))

(define (analyze-and exps)
  [let ([fexps (map analyze exps)])
    (lambda (env)
      (for/and ([x fexps]) (x env)))])

(define (analyze-or exps)
  (let ([fexps (map analyze exps)])
    (lambda (env)
      (for/or ([x fexps]) (x env)))))

(define (analyze-if . args)
  (match (map analyze args)
    [`(,fpred ,fconseq ,falter)
     (lambda (env)
       (if (fpred env)
           (fconseq env)
           (falter env)))]))

(define (analyze-cond pairs)
  (define (foo pa)
    (match pa
      [`(else . ,bodys) `(else  ,(analyze-begin bodys))]
      [`(,pred . ,bodys) #:when (not (null? bodys)) `(,(analyze pred)  ,(analyze-begin bodys))]
      [`(,pred) (analyze pred)]
      [_ (error "Error: analyze-cond")]))
  (let ([fpairs (map foo pairs)])
    (lambda (env)
      (define (scan e)
        (if (null? e) (void)
            (match (first e)
              [`(else ,fbody) (fbody env)]
              [`(,fpred ,fbody) (if (fpred env) (fbody env) (scan (rest e)))]
              [fpred (let ([rv (fpred env)]) (if rv rv (scan (rest e))))])))
      (scan fpairs))))

(define (self-evaluating? e)
  (or (number? e) (string? e) (boolean? e)))

(define (analyze e)
  (match e
    [e #:when (self-evaluating? e) (lambda (env) e)]
    [`',x (lambda (env) x)]
    [`(begin . ,bodys) (analyze-begin bodys)]
    [`(apply ,proc . ,args) (analyze-apply proc args)] 
    [`(lambda ,syms . ,bodys) (analyze-lambda syms bodys)]
    [`(set! ,sym ,exp) (analyze-set! sym exp)]
    [`(define (,proc . ,syms) . ,bodys) (analyze-define proc (analyze-lambda syms bodys))]
    [`(define ,sym ,exp) (analyze-define sym (analyze exp))]
    [`(let ,pairs . ,bodys) (analyze-let pairs bodys)]
    [`(and . ,exps) (analyze-and exps)]
    [`(or . ,exps) (analyze-or exps)]
    [`(if ,pred ,conseq ,alter) (analyze-if pred conseq alter)]
    [`(cond . ,pairs) (analyze-cond pairs)]
    [`(,proc . ,args) (analyze-apply proc args)]
    [x (lambda (env) (env-look-up env x))]))

(define (my-eval e)
  ((analyze e) primitive-env))

; ======================================================================

(define (driver)
  (let ([a (read)])
    (when (not (eq? a eof))
      (let ([rv (my-eval a)])
        (when (not (eq? rv (void))) (displayln rv))
        (driver)))))
(driver)