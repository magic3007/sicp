#lang racket

(require scheme/mpair)

(define mcaar (compose1 mcar mcar))

(define (map-second proc lst)
  (map (lambda x x) (map first lst) (map (compose1 proc second) lst)))

; ======================================================================

(define (make-frame syms vals)
  (mmap mcons (list->mlist syms) (list->mlist vals)))

(define (make-frame-from-pairs pairs)
  (make-frame (map first pairs) (map second pairs)))

(define (frame-look-up frame sym)
  (let ([rv (massoc sym frame)])
    (if rv (mcdr rv) (void)))) 

(define (frame-set-val! frame sym val)
  (let ([rv (massoc sym frame)])
    (if rv (begin (set-mcdr! rv val) #t) #f)))

; ======================================================================

(struct EnvType ([frame #:mutable] ex-env))

(define (env-look-up env sym)
  (if (null? env) (error "unbounded symbol:" sym)
      (let ([rv (frame-look-up (EnvType-frame env) sym)])
        (if (eq? rv (void))
            (env-look-up (EnvType-ex-env env) sym)
            rv))))

(define (env-define env sym val)
  (set-EnvType-frame! env
                       (mcons (mcons sym val) (EnvType-frame env))))

(define (env-set-val! env sym val)
  (if (null? env) (error "unbounded symbol:" sym)
      (let ([rv (frame-set-val! (EnvType-frame env) sym val)])
        (if rv rv
            (env-set-val! (EnvType-ex-env env) sym val)))))

; ======================================================================

(define (syms->frame syms)
  (if (null? syms) '()
      (mmap (lambda (sym)
              (if (pair? sym)
                  (mcons (car sym) (cadr sym))
                  (mcons sym (eval sym (make-base-namespace)))))
            (list->mlist syms))))

(define primitive-frame
  (syms->frame
   `(+ - > < / * = >= <= quotient remainder sqrt cons car cdr assoc
       eq? number? symbol? not list cadr caddr error length displayln display
       append pair? null? (false #f) (true #t) (() (void)))))

(define primitive-env (EnvType primitive-frame '()))

; ======================================================================

(define (call-fproc fproc vals)
  (lambda (env)
    (match (fproc env)
      [(ProcedureType syms fbody proc-env)
       (fbody (EnvType (make-frame syms vals) proc-env))]
      [primitive-proc (apply primitive-proc vals)]
      [_ (error "Error: analyze-apply -- unknown proc")])))

(define (analyze-begin bodys)
  (let ([fbodys (map analyze bodys)])
    (define (scan e)
      (let* ([fi (first e)]
             [re (rest e)])
      (if (null? re) fi
          (lambda (env) (fi env) ((scan re) env)))))
    (scan fbodys)))

(define (analyze-set! sym exp)
  (let ([fexp (analyze exp)])
    (lambda (env)
      (env-set-val! env sym (fexp env))
      (void))))

(define (analyze-define sym analyzed-code)
  (lambda (env)
    (env-define env sym (analyzed-code env))))

(struct ProcedureType (syms fbody env))

(define (analyze-lambda syms bodys)
  (let ([fbody (analyze-begin bodys)])
    (lambda (env)
      (ProcedureType syms fbody env))))

(define (analyze-apply proc args)
  (let ([fproc (analyze proc)]
        [fargs (map analyze args)])
    (lambda (env)
      (let ([vals (map (lambda (x) (x env)) fargs)])
        ((call-fproc fproc vals) env)))))

(define (analyze-and bodys)
  (let ([fbodys (map analyze bodys)])
    (lambda (env)
      (define (scan e)
        (let ([rv ((first e) env)])
          (if rv
              (if (null? (rest e)) rv (scan (rest e)))
              #f)))
      (scan fbodys))))

(define (analyze-or bodys)
  (let ([fbodys (map analyze bodys)])
    (lambda (env)
      (define (scan e)
        (let ([rv ((first e) env)])
          (if rv rv
              (if (null? (rest e)) #f (scan (rest e))))))
      (scan fbodys))))

(define (analyze-if . args)
  (match (map analyze args)
    [`(,fpred ,fconseq ,falter)
     (lambda (env)
       (if (fpred env) (fconseq env) (falter env)))]))

(define (analyze-cond pairs)
  (define (foo pa)
    (match pa
      [`(else . ,bodys) `(else  ,(analyze-begin bodys))]
      [`(,test => ,recipient) `(,(analyze test) => ,(analyze recipient))]
      [`(,pred . ,bodys) #:when (not (null? bodys)) `(,(analyze pred)  ,(analyze-begin bodys))]
      [`(,pred) (analyze pred)]
      [_ (error "Error: analyze-cond")]))
  (let ([fpairs (map foo pairs)])
    (lambda (env)
      (define (scan e)
        (if (null? e) (void)
            (match (first e)
              [`(else ,fbody) (fbody env)]
              [`(,ftest => ,frecipient) (let ([rv (ftest env)])
                                          (if rv
                                              ((call-fproc frecipient `(,rv)) env)
                                              (scan (rest e))))]
              [`(,fpred ,fbody) (if (fpred env) (fbody env) (scan (rest e)))]
              [fpred (let ([rv (fpred env)]) (if rv rv (scan (rest e))))])))
      (scan fpairs))))

(define (analyze-let pairs bodys)
  (let ([fpairs (map-second analyze pairs)]
        [fbody (analyze-begin bodys)])
    (lambda (env)
      (fbody (EnvType
              (make-frame-from-pairs (map-second (lambda (x) (x env)) fpairs))
               env)))))

(define (analyze-while pred bodys)
  (let ([fpred (analyze pred)]
        [fbody (analyze-begin bodys)])
    (lambda (env)
      (define (loop)
        (when (fpred env) (begin (fbody env) (loop))))
      (loop))))

(define (analyze-switch pred pairs)
  (define (foo e)
    (match e
      [`default `default]
      [e (analyze e)]))
  (let* ([fpred (analyze pred)]
         [fpairs (map (lambda x x)
                      (map (compose1 foo first) pairs)
                      (map (compose1 analyze-begin rest) pairs))])
    (lambda (env)
      (let ([rv (fpred env)])
        (define (scan e)
          (if (null? e) (void)
              (match (first e)
                [`(default ,fbody) (fbody env)]
                [`(,fcand ,fbody) #:when (equal? rv (fcand env))
                                  (fbody env)]
                [_ (scan (rest e))])))
        (scan fpairs)))))

(define (analyze-for init pred step bodys)
  (let ([finit (analyze init)]
        [fpred (analyze pred)]
        [fstep (analyze step)]
        [fbodys (map analyze bodys)])
     (lambda (env)
       (define (scan e)
         (cond [(null? e) (void)]
               [(eq? `break ((first e) env)) `break]
               [else (scan (rest e))]))
       (define (loop)
         (if (and (fpred env) (not (eq? `break (scan fbodys))))
           (begin (fstep env) (loop))
           (void)))
       (finit env)
       (loop))))
        
(define (self-evaluating? e)
  (or (number? e) (string? e) (boolean? e)))

(define (analyze e)
  (match e
    [e #:when (self-evaluating? e) (lambda (env) e)]
    [`',x (lambda (env) x)]
    [`(set! ,sym ,exp) (analyze-set! sym exp)]
    [`(begin . ,bodys) (analyze-begin bodys)]
    [`(lambda ,args . ,bodys) (analyze-lambda args bodys)]
    [`(apply ,proc ,args) (analyze-apply proc args)]
    [`(and . ,bodys) (analyze-and bodys)]
    [`(or . ,bodys) (analyze-or bodys)]
    [`(cond . ,pairs) (analyze-cond pairs)]
    [`(while ,pred . ,bodys) (analyze-while pred bodys)]
    [`(for ,init ,pred ,step . ,bodys) (analyze-for init pred step bodys)]
    [`(break) (lambda (env) `break)]
    [`(switch ,pred . ,pairs) (analyze-switch pred pairs)] 
    [`(let ,pairs . ,bodys) (analyze-let pairs bodys)]
    [`(if ,pred ,conseq ,alter) (analyze-if pred conseq alter)]
    [`(define (,proc . ,args) . ,bodys)
     (analyze-define proc (analyze-lambda args bodys))]
    [`(define ,sym ,exp) (analyze-define sym (analyze exp))]
    [`(,proc . ,args) (analyze-apply proc args)]
    [x (lambda (env) (env-look-up env x))]))

(define (my-eval e)
  ((analyze e) primitive-env))

; ======================================================================

(define (check e ans)
  (let ([rv (my-eval e)])
    (when (not (equal? rv ans))
      (begin
        (display "expected: ")
        (displayln ans)
        (display "given: ")
        (displayln rv)
        (error "assertion failed")))))

(define ns (make-base-namespace))
(for-each (lambda (x) (check (first x) (second x))) 
          `(
            ((cond (3 => (lambda (x) (+ x x)))) 6)
             ))

; ======================================================================

(define (driver)
  (let ([a (read)])
    (when (not (eq? a eof))
      (let ([rv (my-eval a)])
        (when (not (eq? rv (void))) (displayln rv))
        (driver)))))
(driver)