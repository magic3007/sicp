#lang racket

;; An iterator is either the "done" iterator or a pair with the 
;; current value of the iterator and a continuation which returns
;; the next value of the iterator when evaluated.
;; Type Iterator<T> = done | Pair(T, (Empty -> Iterator<T>))

;; Purpose: The yield Iterator constructor takes 2 arguments:
;; - The value to return to the caller
;; - The continuation to be executed when the Iterator is resumed (by invoking next())
;; Signature: yield(result, continuation)
;; Type: [T * [Empty -> Iterator<T>] -> Iterator<T>]


;; An iterator has 2 fields:
;; - Next: execute the next step of the computation of the generator
;; - Value: access the current value of the generator
;; And a state predicate 
;; - done?: determines whether the generator has reached the end of the 
;;          computation.

;; The accessors are iter->next, iter->value and iter->done?
;; iter->next computes the next step of the iterator.

;; Iter->next is a constructor - it returns the value of the next
;; iterator by invoking the continuation.
;; Once an iterator is done, next keeps returning done.
;; Else - next computes the next step of the iterator by invoking the continuation.
