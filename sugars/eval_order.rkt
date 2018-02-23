#lang racket

(define-syntax (Inside stx)
  (syntax-case stx ()
    [(Inside x) #'"inside"]))

(define-syntax (Outside stx)
  (syntax-case stx (Inside)
    [(Outside (Inside x)) #'"outside"]))

(Outside (Inside 3))


(define-syntax (Inner stx)
  (println "inner")
  (syntax-case stx ()
    [(Inner x) #'x]))

(define-syntax (Outer stx)
  (println "outer")
  (syntax-case stx ()
    [(Outer x) #'x]))

(Outer (Inner 3))