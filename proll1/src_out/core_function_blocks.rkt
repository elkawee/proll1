(module function_blocks racket
        (require "var_and_sub.rkt" )

        
        (struct goal ())
        (struct @s goal ()) ; endpoint 
        (struct @u goal ()) ; endpoint
        (struct @E () ); designates an invalid substitution (accompanied with @u in car position )
        (struct goal-c goal (Gf S alt )
                )
        
        


        
        (define-syntax goalify (syntax-rules  () 
                                 [(_ expr ) 
                                  (lambda (S)
                                    (goal-c expr S (@u)))]))
        
        (provide goalify ) 
                         
        
        ; tree filtering macro? 
        (define (exposed-query X S )
          (query X S))
        
        
          
        (define-syntax letq (syntax-rules ()
                              [(_ ([V Exp]) body ... )
                               (lambda (S)
                                 (let* ([ V (Var "lolq" 1) ]
                                        [ S2 (ext-s V (q-filter S Exp) S )])
                                   (goal-c (make-and-G body ... ) S2 (@u))))
                               ]))
        
        (define-syntax q-filter (syntax-rules (qry q-filter)
                                  [(q-filter S (qry X)) (exposed-query X S)]
                                  [(q-filter S (X1 Xrest ... )) ( (q-filter S X1) (q-filter S Xrest) ... ) ]
                                  [(q-filter S Leaf) Leaf ] ))
        
        (provide letq)
        

        ; also ( (@u) _ (@s) ) darf nie vorkommen nur ( (@s) _ (@u) ) und ( (@u) _ (@u) )
        
        (define (run*simple F S ) 
          (match* (F S)
                  [( (@u) _ ) '() ]
                  [( (@s) (sub)) (list S) ]
                  [( (? procedure?) (sub) )
                   (match (F S)
                          [ (goal-c F1 S_ F2 )
                            (append (run*simple F1 S_ ) (run*simple F2 S_ )) ] ) ]))
                            
        ; ---------------------------------- 
        (define (run-limit-simple Lim F S )
          (cond ((Lim . equal? . 0 )  '() )
                (else (match* (F S)
                              [ ( (@u) _ ) '() ]
                              [ ( (@s) (sub) ) (list S ) ] 
                              [ ( (? procedure?) (sub) )
                                ( match (F S)
                                        [ (goal-c F1 S/ F2)
                                          (let* ([L1 (run-limit-simple Lim F1 S/)]
                                                 [len1 (length L1)]
                                                 [Lim2 (- Lim len1)])
                                            (append L1 (run-limit-simple Lim2 F2 S/))) ])]))))
                

          
  
        ; make-*** functions all produce (lambda(S) ..) -> goal 

        (define (make=:= X1 X2 )
          (lambda (S ) 
            (match (unify X1 X2 S)
                   ['fail 
                    (goal-c (@u) (@E) (@u))]
                   [(and S_ (? sub?)) 
                    (goal-c (@s) S_ (@u))])))
  
  

        ; [ (S -> G ) ] -> ( S ->G )
        ; im falle von (@u) is rueckgabe-S egal? 
        (define make-and-G (lambda  G-list
                             ;(printf "G-list ~s~n" G-list )
                             (match G-list
                                    [(cons Fh '() )
                                     (lambda (S) 
                                       (Fh S))]
                                    [(cons Fh Frest) 
                                     (lambda (S)
                                       (match (Fh S)
                                              [(and (goal-c (@u) _ (@u)) X )  X ]
                                              [(goal-c (@s) S_ (@u) ) (goal-c (apply make-and-G Frest) 
                                                                           S_ 
                                                                           (@u) )]
                                              [(goal-c (@s) S_ (and f2 (? procedure?))) 

                                               (goal-c (apply make-and-G Frest)
                                                       S_
                                                       (apply make-and-G (cons f2 Frest)))]
                                               
                                              [(goal-c (and f1 (? procedure? )) S_ (and f2 (? procedure? ))) 
                                               
                                               (goal-c (apply make-and-G (cons f1 Frest)) 
                                                       S_ 
                                                       (apply make-and-G (cons f2 Frest)) )]
                                              [(goal-c (and f1 (? procedure?)) S_ (@u))
                                               (goal-c (apply make-and-G (cons f1 Frest))
                                                       S_
                                                       (@u))]

                                              
                                       ))])))
                                     
        ; non declared variable schlaegt auf scheme ebene fehl 
        
        (define (make-fresh-s V chF)
          ;(printf "making fresh: ~s ~s ~n" V chF ) 
          (lambda (S)
            (let ([S_ (sub-comp 
                       (elem-sub V (empty)) 
                       S)])
              (goal-c chF S_ (@u)))))
        
        
        ; the (question -> consequense ) fromat in reasoned's cond-e doesnt make much sense anyway
        ; so we'll enforce stuff like ( cond (and g1 g2 ...) (and g3 g4 ...) ) usw, which has the same semantic 
        ; -> the arg is a list :  [ S-> G] 
        (define make-cond-G  (lambda pair-list
                               (match pair-list
                                      [(cons F1 '()) 
                                       (lambda (S)
                                         (F1 S))]
                                      [(cons F1 Frest)
                                       (lambda (S) 
                                         (goal-c F1 S (apply make-cond-G Frest)))])))
        
        (define (make-not-G F)
          (lambda (S)
            (match (F S)
                   ; throw away all bindings -> loop original sub through
                   ; ( not (cond g1 g2 ...)) means non of the goals are supposed to match 
                   ; it never produces alternatives 
                   [ (goal-c (@u) S_ f2 ) (match f2
                                        ;                                                [(? procedure?) (goal-c (make-not-G f2) S (@u))] ; kommt per def nicht vor
                                                 [(@u)          (goal-c (@s) S (@u))]
                                                 [(@s)          (goal-c (@u) S (@u))])]
                   [ (goal-c (@s) _ _ ) (goal-c (@u) S (@u)) ]

                   [ (goal-c (and f1 (? procedure?)) S_ f2) (match f2 
                                                                   [(@s) (goal-c (@u) S (@u))]
                                                                   ; wir brauchen hier einen stack - es muss das originale S zurueckgegeben werden waehrend das naechste goal die bindings aus S_ braucht 
                                                                   [(@u) 
                                                                    (goal-c (make-not-G 
                                                                             (lambda (S_null) (f1 S_)))
                                                                             S
                                                                             (@u))]
                                                                   [(? procedure?)
                                                                    (goal-c (make-not-G (lambda (S_null) 
                                                                                          ((make-and-G f1 f2) S_)
                                                                                                ))
                                                                            S
                                                                            (@u))
                                                                   ])])))

                                                                    
                                                                                 
                                                  
        
        (provide (contract-out 
                  [make-cond-G (((-> sub? goal?) )
                                #:rest (listof (-> sub? goal?)) 
                                . ->* .
                                (-> sub? goal?) ) ]
                  [make-and-G (->* 
                               ((-> sub? goal?) ) 
                               #:rest (listof (-> sub? goal?)) 
                               (-> sub? goal?) ) ]
                  [make-not-G (->
                               (-> sub? goal?)
                               (-> sub? goal?))]
                  [make-fresh-s (-> 
                                 Var? 
                                 (-> sub? goal?) 
                                 (-> sub? goal?) ) ]
                  [make=:= (-> 
                            (or/c Var? RVal?) 
                            (or/c Var? RVal?)
                            (-> sub? goal?)) ]
                  [run*simple (-> (sub? . -> . goal? ) sub? (listof sub? )) ]
                  [run-limit-simple (-> number? (sub? . -> . goal? ) sub? (listof sub? )) ]
                  
                  [struct goal () ]

                  ))
        
)
