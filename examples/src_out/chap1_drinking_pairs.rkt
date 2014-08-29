#lang racket
(require (file "../../proll1/src_out/proll1.rkt"))

(define (drinks P D)
  (cond-e 
   [(=:= P 'john)(=:= D 'martini)]
   [(=:= P 'mary)(=:= D 'gin )]
   [(=:= P 'susan)(=:= D 'wodka)]
   [(=:= P 'john) (=:= D 'gin)]
   [(=:= P 'fred) (=:= D 'gin)]
   [(=:= P 'james-b) (=:= D 'martini)]))

(define (pair X Y)
  (fresh (Z)
         (and-g (drinks X Z) 
                (drinks Y Z) 
                (not-g (=:= X Y)))))

(run* (P)
      (pair 'john P))


