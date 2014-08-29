
(module macro_blocks racket 
        
        (provide run*)
        (provide run ) 
        (provide fresh)
        (provide =:= )
        (provide cond-e )
        (provide and-g )
        (provide not-g )

        ; ---- impl --- ;
        (require "var_and_sub.rkt" )
        (require "core_function_blocks.rkt" )

        
  
        (define-syntax fresh (syntax-rules ()
                               [ (_ (V) body ... )
                                 (new-var-let V
                                   (make-fresh-s V (and-wrap body ... )))]
                               [ (_ (V1 V2 Vrest ...) body ...)
                                 (fresh (V1) (fresh (V2 Vrest ... ) body ... )) ]
                               ))

        (define-syntax fresh-sub-only (syntax-rules () 
                                        [( _ (V) B1 body ... ) 
                                         (make-fresh-s V (and-wrap B1 body ... )) ]
                                        [( _ (V1 V2 Vrest ... ) body ... )
                                         (make-fresh-s V1 (fresh-sub-only (V2 Vrest ...) body ...  ) ) ] 
                                        ))

        (define-syntax =:= (syntax-rules ()
                             [ (_ V1 V2) (make=:= V1 V2 )] ))
        

        (define-syntax run* (syntax-rules () 
                              [ (_ (var-list ... ) body ... )
                                (new-var-let* ( var-list ... )
                                (let ([ X (run*simple 
                                           (fresh-sub-only (var-list ... ) body ... ) 
                                           (empty-sub))])
                                  (map (lambda (R)  (query-multi R var-list ... )) X )))]))

        (define-syntax run (syntax-rules () 
                             [ (_ Count (var-list ... ) body ... )
                               (new-var-let* ( var-list ... ) 
                                             (let ([ X (run-limit-simple Count 
                                                                         (fresh-sub-only (var-list ... ) body ... )
                                                                         (empty-sub))])
                                               (map (lambda(R) (query-multi R var-list ... )) X )))]))


        (define-syntax cond-e (syntax-rules ()
                                [(_  Row ... )
                                 (make-cond-G  (cond-row Row )  ... ) ]))
        

        (define-syntax cond-row (syntax-rules () 
                                  [(_ ( body ...) ) (and-wrap body ... )]))

        
        (define-syntax and-g (syntax-rules () 
                               [(_ body ... ) (and-wrap body ... )]))
        (define-syntax not-g (syntax-rules () 
                               [(_ arg) (make-not-G arg)]))

        
        ; --- aux macros --- ; 

        (define-syntax query-multi (syntax-rules ()
                                       [ (_ Sub V ... ) (list  (query V Sub) ... ) ]
                                       ))

        (define-syntax and-wrap  (syntax-rules () 
                                   [ (_ body) body ] 
                                   [ (_ B1 body ... ) (make-and-G B1 body ... ) ] ))
  
        (define-syntax new-var-let (syntax-rules () 
                                     [ (_ V body ... )
                                       (let ([V (Var "lol" 1 )])
                                         body ... )]))
        
        (define-syntax new-var-let* (syntax-rules () 
                                      [ (_ (V1 ) body ... )
                                        (new-var-let V1 body ... ) ]
                                      
                                      [ (_ (V1 V2 Vrest ... ) body ... )
                                        (new-var-let V1 
                                                     (new-var-let* (V2 Vrest ... ) 
                                                                   body ... )) 
                                        ] ))
        


)
