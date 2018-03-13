#lang racket

(define-syntax (outer stx)
  (syntax-case stx ()
    [(outer x)
     (begin
       (printf "Begin outer call\n")
       (define answer #'(* x 2))
       (printf "End outer call\n")
       answer)]))

(define-syntax (inner stx)
  (syntax-case stx ()
    [(inner x)
     (begin
       (printf "Begin inner call\n")
       (define answer #'(+ x 1))
       (printf "End inner call\n")
       answer)]))

(printf "(10 + 1) * 2 = ~a\n" (outer (inner 10)))

#|
Output:
  Begin outer call
  End outer call
  Begin inner call
  End inner call
  (10 + 1) * 2 = 22
Notice that:
  1. The calls aren't nested, so this isn't simply evaluation.
  2. The outer call is first: this is OI order.
  3. The result is 22, not 12.
     This doesn't show anything, though, since Racket has parenthetical syntax.
|#