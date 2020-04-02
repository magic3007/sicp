#lang racket

(define (A)
  (let ([x 0]
        [state `unconstructed])
    (define (foo a)
      (define (setx a)
        (if (eq? state `unconstructed)
            (displayln "object hasn't been constructed!")
            (set! x a)))
      (if (eq? state `destructed)
          (displayln "object has been destructed!")
          (match a
            [(? number?)
              (if (eq? state `constructed)
                  (displayln "object has been constructed!")
                  (begin (set! x a) (set! state `constructed)))]
            ['delete (set! state `destructed)]
            ['setx setx]
            ['getx
              (if (eq? state `unconstructed)
                  (displayln "object hasn't been constructed!")
                  x)])))
    foo))

(define (delete var)
  (var `delete))

(define a (A))
(a 2)
(a 4)
(display (a 'getx))
(newline)
((a 'setx) 1)
(display (a 'getx))
(newline)
(define b (A))
((b 'setx) 2)
(b 5)
(display (b 'getx))
(newline)
(delete a)
(delete b)
(delete a)
(b 'getx)