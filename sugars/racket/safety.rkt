#lang typed/racket

(define-syntax (ill-typed stx)
  (syntax-case stx ()
    [(ill-typed)
     (+ 1 "two")]))
; No type error -- not type safe!

(define-syntax (ill-scoped stx)
  (syntax-case stx ()
    [(ill-scoped)
     #'x]))
; No unbound id error - not scope safe!

(define-syntax (scope-safe stx)
  (syntax-case stx ()
    [(scope-safe v body)
     #'(let [(x "macro")]
         (let [(v "user")]
           (printf "macro->~a\n" x)
           body))]))

(scope-safe x (printf "user->~a\n" x))
; Hygienic -- user vars and macro vars don't clash.
; (This only scratches the surface of the kinds of nonsense
;  that Racket macros deal with hygienically.)

(define-syntax (syntax-unsafe stx)
  (syntax-case stx ()
    [(syntax-unsafe)
     #'(let (x 1) x)]))
; Oops, forgot a set of parens.
; Won't find out until running the macro.
; Syntax unsafe. (It is paren-safe, though.)
