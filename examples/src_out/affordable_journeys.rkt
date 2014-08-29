#lang racket 
(require (file "../../proll1/src_out/proll1.rkt"))
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

(define border (make_pred/2 '([ sussex kent ]
                              [ sussex surrey]
                              [ surrey kent ]
                              [hampshire sussex]
                              [hampshire surrey]
                              [hampshire berkshire]
                              [berkshire surrey]
                              [wiltshire hampshire]
                              [wiltshire berkshire]
                              )))
(define (adjacent X Y )
  (cond-e 
   [ (border X Y )]
   [ (border Y X) ]))


(define (affordable X Y)
  (fresh (Z) 
         (goalify (adjacent X Z))
         (goalify (adjacent Z Y))
         (not-g (=:= X Y))
         ))



(printf "===========~n~s~n============" (run* (V W) (affordable V W)))
