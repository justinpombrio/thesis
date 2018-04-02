#lang r5rs

(define-syntax ill-scoped
  (syntax-rules ()
    ((ill-scoped)
     x)))
; No unbound id error - not scope safe!

(define-syntax hygienic
  (syntax-rules ()
    ((hygienic v body)
     (let ((x "macro"))
       (let ((v "user"))
         (display "macro->") (display x) (newline)
         body)))))

(hygienic x (begin (display "user->") (display x) (newline)))
; Hygienic -- user vars and macro vars don't clash.
; (This only scratches the surface of the kinds of nonsense
;  that Racket macros deal with hygienically.)

(define-syntax syntax-unsafe
  (syntax-rules ()
    ((syntax-unsafe)
     (let (x 1) x))))
; Oops, forgot a set of parens.
; Won't find out until running the macro.
; Syntax unsafe. (It is paren-safe, though.)
#;(syntax-unsafe)