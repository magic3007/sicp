#lang racket
; http://lisp.test.openjudge.org/2020hw11/2/

(require scheme/mpair)

; ======================================================================

(struct TrunkType (e cur-env))
(struct MemoTrunkType (e cur-env [evaluated #:mutable] [rv #:mutable]))

(define (force-it val)
  (match val
    [(TrunkType e cur-env) ((force-analyze e) cur-env)]
    [(MemoTrunkType e cur-env evaluated _)
     (begin
       (when (not evaluated)
         (begin
           (set-MemoTrunkType-evaluated! val #t)
           (set-MemoTrunkType-rv! val ((force-analyze e) cur-env))))
       (MemoTrunkType-rv val))]
    [val val]))

; ======================================================================

(define (make-frame syms vals)
  (mmap mcons (list->mlist syms) (list->mlist vals)))

(define (make-frame-from-pairs pairs)
  (make-frame (map first pairs) (map second pairs)))

(define (frame-look-up frame sym)
  (let ([rv (massoc sym frame)])
    (if rv
        (values (mcdr rv) #t)
        (values (void) #f))))

(define (frame-set-val! frame sym val)
  (let ([rv (massoc sym frame)])
    (if rv (begin (set-mcdr! rv val) #t) #f)))

; ======================================================================

(struct EnvType ([frame #:mutable] ex-env))

(define (env-look-up env sym)
  (if (null? env) (error "unbounded symbol:" sym)
      (let-values ([(rv found?) (frame-look-up (EnvType-frame env) sym)])
        (if found? rv
            (env-look-up (EnvType-ex-env env) sym)))))

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
       eq? number? symbol? not list cadr caddr error length displayln display newline
       append pair? null? void (false #f) (true #t))))

(define primitive-env (EnvType primitive-frame '()))

; ======================================================================

(struct ProcedureType (syms fbody env))

(define (run-fproc fproc args env)
  (define (foo sym arg)
    (match sym
      [`(,sym lazy) `(,sym ,(TrunkType arg env))]
      [`(,sym lazy-memo) `(,sym ,(MemoTrunkType arg env #f #f))]
      [sym `(,sym ,((force-analyze arg) env))]))
  (match ((force-fexp fproc) env)
    [(ProcedureType syms fbody proc-env)
     (fbody (EnvType (make-frame-from-pairs (map foo syms args)) proc-env))]
    [primitive-proc
     (let* ([vals (map (lambda (x) ((force-analyze x) env)) args)])
       (apply primitive-proc vals))]))

; ======================================================================

(define (analyze-lambda syms bodys)
  (let ([fbody (analyze-begin bodys)])
    (lambda (env)
      (ProcedureType syms fbody env))))

(define (analyze-apply proc args)
  (let ([fproc (analyze proc)])
    (lambda (env) (run-fproc fproc args env))))

(define (analyze-let pairs bodys)
  (let* ([syms (map first pairs)]
         [args (map second pairs)]
         [fproc (analyze-lambda syms bodys)])
     (lambda (env) (run-fproc fproc args env))))

(define (analyze-begin bodys)
  (let ([fbodys (map analyze bodys)])
    (lambda (env)
      (for/last ([x fbodys]) (x env)))))

(define (analyze-set! sym exp)
  (let ([fexp (analyze exp)])
    (lambda (env)
      (env-set-val! env sym (fexp env))
      (void))))

(define (analyze-define sym analyzed-code)
  (lambda (env)
    (env-define env sym (analyzed-code env))))

(define (analyze-and bodys)
  (let ([fbodys (map analyze bodys)])
    (lambda (env)
      (for/and ([x fbodys]) ((force-fexp x) env)))))

(define (analyze-or bodys)
  (let ([fbodys (map analyze bodys)])
    (lambda (env)
      (for/or ([x fbodys]) ((force-fexp x) env)))))

(define (analyze-if . args)
  (match (map analyze args)
    [`(,fpred ,fconseq ,falter)
     (lambda (env)
       (if ((force-fexp fpred) env)
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
              [`(,fpred ,fbody) (if ((force-fexp fpred) env)
                                    (fbody env)
                                    (scan (rest e)))]
              [fpred (let ([rv ((force-fexp fpred) env)]) (if rv rv (scan (rest e))))])))
      (scan fpairs))))

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
    [`(if ,pred ,conseq ,alter) (analyze-if pred conseq alter)]
    [`(let ,pairs . ,bodys) (analyze-let pairs bodys)]
    [`(define (,proc . ,syms) . ,bodys)
     (analyze-define proc (analyze-lambda syms bodys))]
    [`(define ,sym ,exp) (analyze-define sym (analyze exp))]
    [`(,proc . ,args) (analyze-apply proc args)]
    [x (lambda (env) (env-look-up env x))]))

(define (force-fexp fe)
  (lambda (env)
    (force-it (fe env))))

(define (force-analyze e)
  (force-fexp (analyze e)))

(define (my-eval e)
  ((force-analyze e) primitive-env))

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

(for-each (lambda (x) (check (first x) (second x))) 
          `(
             ))

; ======================================================================

(define (driver)
  (let ([a (read)])
    (when (not (eq? a eof))
      (let ([rv (my-eval a)])
        (when (not (eq? rv (void))) (displayln rv))
        (driver)))))
(driver)