#lang racket

;  美元有1分,5分,10分,25分,50分这几种硬币。给定一一个金额,求用 硬币凑出该金额的方法数.

(define (count-change amount)
  (cc amount 5))

(define first-denomination (vector 0 1 5 10 25 50))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (vector-ref first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(displayln (count-change 100))