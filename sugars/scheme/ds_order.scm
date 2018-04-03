#lang r5rs

(define-syntax outer
  (syntax-rules (+ inner)
    ((outer (+ a b))
     "IO evaluation order")
    ((outer (inner x))
     (* (inner x) 2))))

(define-syntax inner
  (syntax-rules ()
    ((inner x)
     (+ x 1))))

(display "(10 + 1) * 2 = ")
(display (outer (inner 10)))
(newline)

#|
Output:
  (10 + 1) * 2 = 22
Notice that:
  1. When the outer call happens, the inner call is there: this is OI order.
  2. The result is 22, not 12.
     This doesn't show anything, though, since Scheme has parenthetical syntax.
|#