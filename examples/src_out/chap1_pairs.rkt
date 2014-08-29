  #lang racket
  (require (file "../../proll1/src_out/proll1.rkt"))
  
  (define (male X)
    (cond-e
     [(=:= X 'bert)]
     [(=:= X 'parcival)]
     ))
  
  (define (female X)
    (cond-e
     [(=:= X 'lucinda)]
     [(=:= X 'camilla)]))
  
  (define (pair X Y ) 
    (cond-e
     [(male X )  (female Y)]
     [(female X) (male   Y)]))
  
  
  (run* (V1 V2 )
        (pair V1 V2)
)