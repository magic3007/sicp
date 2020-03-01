#lang racket

(define (average a b)
  (/ (+ a b) 2))

(define threshold 1e-6)

(define (close-enough? a b)
  (< (abs (- a b)) threshold))

(define (cube x)
  (* x x x))


; ==========================================
; buildin procedure
; More info in: https://docs.racket-lang.org/reference/
; ==========================================

; [extra constants and functions]
; pi pi.f
; (degrees->radians x) (radians->degree x)
; sqr
; sgn
; conjugate

; even? odd?
; zero? positive? negative?

; (quotient n m)
; (remainder n m): return the same sign as n
; (modulo n m): return the same sign as m
; (quotient/remainder n m) → integer? integer?

; numerator denominator
; 
; add1 sub1
; abs
; max/min
; gcd lcm
; 
; (round x): Returns the integer closest to x:real
; floor ceil truncate
; exact-round exact-floor exact-ceiling exact-truncate: return exact integer
; 
; (numerator q) (denominator q): Coerces q to an exact number,  finds the numerator
;     or demominatorof the number expressed in its simplest fractional form

; (rationalize x tolerance): Among the real numbers within (abs tolerance) of x, returns the one
;     corresponding to an exact number whose denominator is the smallest.
; > (rationalize 1/4 1/10)
;   1/3


; sqrt
; (integer-sqrt n) = (floor (sqrt n))
; (integer-sqrt/remainder n)
; 
; (expt z w): return z raised to the power of w
; (exp z): return e^z
; (log z [b])
; sin cos tan asin acos atan

; [complex number operations]
; (make-rectangular x y): return x+yi
; (make-polar magnitude angle)
; real-part image-part magnitude angle

; [bitwise operations]
; bitwise-ior / bitwise-and / bitwise-xor / bitwise-not
; bitwise-bit-set?



