#lang racket 
(require (file "../../proll1/src_out/proll1.rkt"))

(define (length1 L N)
  (cond-e 
   [(=:= L '()) (=:= N 0)]
   [(fresh (H T N/)
           (=:= L (cons H T))
           (goalify (length1 T N/))
           (letq ([N// (+ (qry N/) 1 )])
                 (=:= N N//)))]))

;(run 1  (X) (length1  (cons 'a X) 3))


(define (length2 L N )
  (fresh (dummy)
         (=:= dummy 0)
         (accum L N dummy)))


(define (accum L N A)
  (cond-e 
   [(=:= L '()) (=:= N A ) ]
   [(fresh (H T) 
           (=:= L (cons H T ))
           (letq ([A~ (+ (qry A) 1 ) ])
                 (goalify (accum T N A~))))]))

(run 3 (X  L ) 
     (length2  (cons '1 L ) X ))

