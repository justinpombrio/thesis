#lang racket

; Macros can abstract over any code:
(define-syntax (augment stx)
  (syntax-case stx (define)
    [(augment (define x v))
     #'(define x (+ v 1))]))
(augment (define n 2))
(printf "2+1=~a\n" n)

; Note that (define ...) is not an expression.
#;(+ (define x 1) 2)
; ERROR: 'define: not allowed in an expression context'


; Macros can define macros:

(define-syntax (my-macro stx)
  (syntax-case stx ()
    [(my-macro (lhs arg ...) rhs)
     #'(define-syntax (lhs stx)
         (syntax-case stx ()
           [(lhs arg ...) #'rhs]))]))

(my-macro (twice x) (begin x x))

(twice (printf "yeah "))