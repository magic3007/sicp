#lang racket

(define spot 1234)

(set! spot
  (let* ([sc #f]
         [tmp (let/cc cc (set! sc cc) 0)])
    (displayln sc)
    (displayln tmp)
    sc))

(spot 23)
(spot 24)