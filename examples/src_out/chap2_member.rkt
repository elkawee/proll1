#lang racket 
(require (file "../../proll1/src_out/proll1.rkt"))
(define (member X L )
  (fresh (H T)
         (=:= (cons H T) L) ; bricht hier einfach ab, wenn nil nicht mit (cons H T) unified
         (cond-e
          [(=:= X H)]
          [(goalify (member X T))])))

;(run* (X) (member 'a `[ a ,X c d a]))

(define (mystery X L1 L2)
  (and-g (member X L1) (member X L2)))

(run* (X) (mystery X '(a b c) '(c d e a)))
          

