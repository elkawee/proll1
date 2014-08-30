#lang racket
(define (make_pred/2 assoc_list)
  (match assoc_list
         [(cons (list nam1 nam2 ) '())
          (lambda ( X Y )
            (and-g (=:= X nam1 ) (=:= Y nam2)))]
         [(cons (list nam1 nam2) rest)
          (lambda (X Y)
            (cond-e 
             [(=:= X nam1)(=:= Y nam2)]
             [((make_pred/2 rest ) X Y)]
             ))]))
(require (file "../../proll1/src_out/proll1.rkt"))

(define a (make_pred/2 '([g h]
                         [g d]
                         [e d]
                         [h f]
                         [e f]
                         [a e]
                         [a b]
                         [b f]
                         [b c]
                         [f c])))

(define (path X Y )
  (cond-e 
   [(=:= X Y )]
   [(fresh (Z)
           (a X Z)
           (goalify (path Z Y)))]))
 

(run* (X) (path 'g X))
(printf "~n=============================~n" )
(run* (X) (path X 'c))
