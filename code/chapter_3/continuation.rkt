#lang racket

(define exit #f)

(define cont-list '())
(define len (read))

(define (set-cont-list n)
  (define (output n)
    (for-each (lambda (x) (displayln x)) (reverse (build-list (add1 n) add1))))
  (set! cont-list (build-list n
                              (lambda (idx)
                                (let* ([sc #f]
                                       [cc (let/cc cc (set! sc cc) #t)])
                                  (if (false? cc) (output idx) (void))
                                  sc)))))
                        

(define (show n)
  (define (show-helper l n)
    (if (= n 0)
        (if (continuation? (car l))
            ((car l) #f)
            (displayln "error"))
        (show-helper (cdr l) (- n 1))))
  (show-helper cont-list (- n 1)))

(define (main)
  (set-cont-list len)
  (define k (read))
  (if (eq? k eof)
      (void)
      (show k)))

(main)