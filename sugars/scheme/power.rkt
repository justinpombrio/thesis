#lang r5rs

; Macros can abstract over any code:
(define-syntax augment
  (syntax-rules (define)
    ((augment (define x v))
     (define x (+ v 1)))))
(augment (define n 2))
(display "2 + 1 = ") (display n) (newline)

; Note that (define ...) is not an expression.
#;(+ (define x 1) 2)
; ERROR: 'define: not allowed in an expression context'


; Macros can define macros:

(define-syntax my-macro
  (syntax-rules ()
    ((my-macro (lhs arg ...) rhs)
     (define-syntax lhs
       (syntax-rules ()
         ((lhs arg ...) rhs))))))

(my-macro (twice x) (begin x x))

(twice (display "yeah "))