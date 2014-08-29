
(module var_and_sub racket 

        (struct proll-type ())
        (struct Var proll-type (Name line))
        (struct empty proll-type ())
        (define (RVal? X) (not (proll-type? X )))
        

        
        (struct sub proll-type ())
        
        (struct empty-sub sub ())

        (struct elem-sub proll-type (lhs rhs) 
                #:transparent 
                #:guard (lambda (lhs rhs t-name) 
                          (cond 
                           [(and (Var? lhs)
                                 (or (Var? rhs) (RVal? rhs) (empty? rhs)))
                            (values lhs rhs)]
                           [else (error "ladkjflk")])))

        (struct sub-comp sub (e S)
                #:transparent
                #:guard (lambda (e S t-name )
                          (match* (e S) 
                                 [ ( (elem-sub _ _) (or (empty-sub) (sub-comp _ _ )) ) (values e S )]
                                 [ ( _ _ ) (error "") ])))

	(define (ext-s V X Sub)
	  (sub-comp (elem-sub V X ) Sub ))

	(provide ext-s)


        
        (define (assoc  lhs S )
          (cond 
           [(empty-sub? S) 'fail ]
           [(eq? lhs (elem-sub-lhs (sub-comp-e S)))
            (elem-sub-rhs (sub-comp-e S))]
           [else (assoc lhs (sub-comp-S S))]
           ))

        (define (assoc* X S)
          (match (assoc X S)
                 [ 'fail 'fail]
                 [ (and rhs (? Var?)) (assoc* rhs S)]
                 [ R R] ))


        (define (walk X S)
          (cond 
           [(RVal? X) X]
           [else 
            (match (assoc X S ) 
                   [ 'fail 'fail ]
                   [ (and V (Var _ _)) (walk V S) ]
                   [ (and R (? RVal?)) R ]
                   [ (empty) X ])]))

        (define (fresh-f-s V f)
          (lambda (S )
            (f (sub-comp (elem-sub V (empty)) S))))

        (define (unify X Y S )
;          (print-sub S)
          (define (ex L R S )  
            (sub-comp (elem-sub L R ) S ))
          (let ([X_ (walk X S)]
                [Y_ (walk Y S)] )
            (cond
             [(eq? X_ Y_) S]
             [else 
              (match* ( X_ Y_ )
                      [ ( (Var _ _) _ ) (ex X_ Y_ S ) ]
                      [ ( _ (Var _ _) ) (ex Y_ X_ S ) ] 
                      [ ( (cons Xh Xt) (cons Yh Yt))
                        (match (unify Xh Yh S )
                               [ 'fail 'fail ]
                               [ (and S_ (sub)) (unify Xt Yt S_) ])]
                      [ ( _ _ ) (begin 
                                  ;(printf "~s~s~n" X_ Y_ )
                                  (cond 
                                   [(equal? X_ Y_ ) S ]
                                   [else 'fail ]))])])))


        
        (define (print-elem-sub es )
          (let ([lhs (elem-sub-lhs es)]
                [rhs (elem-sub-rhs es)])
            (match lhs
                   [(Var name line) (printf "~s" name)])
            (printf "->")
            (match rhs
                   [(Var name line) (printf "~s" name)]
                   [(? RVal?) (printf "~s" rhs)]
                   [(empty) (printf "|_|")])))
                   
            
        (define (print-sub S)
          (match S
                 [(sub-comp es rest) (begin (print-elem-sub es)
                                            (printf ", ")
                                            (print-sub rest))]
                 [(empty-sub) (printf "()~n")]))
        
        

        (define (query X S) ; macht nicht wirklich sinn weils nicht matchen kann 
          (printf "query for : ~s~n in :" X )
          (print-sub S)
          (match X
                 [ (Var _ _) (query (assoc* X S) S) ]
                 [ (cons H T) (cons (query H S) (query T S )) ]
                 [ (or (? RVal?) (empty) ) X ]))


                          

        (require (for-syntax racket))
        (define-syntax new-var (lambda (stx)
                                 (match-let ([ (list nv name)  (syntax->datum stx)]
                                             [ (list _ sname)  (syntax-e stx)]
                                             )
                                            #`(define #,sname (Var #,(symbol->string name) #,(syntax-line stx)))
                                            )))


        ; --------------------------------- ; 
        (provide new-var)

        (provide (contract-out 
                  [query (-> any/c any/c any/c )]))
        (provide (contract-out
                  [print-sub (-> sub? any/c)]))

        (provide (contract-out
                  [unify (-> (or/c Var? RVal?)  (or/c Var? RVal?) sub? (or/c sub? 'fail ))]))

        (provide (contract-out
                  [ struct sub () ]
                  [ struct sub-comp ((e elem-sub?) (S sub?) ) ]
                  [ struct elem-sub ( (lhs any/c) (rhs any/c ) ) ] 
                  [ struct empty () ] 
                  [ struct empty-sub () ]
                  ))
        
        (provide (contract-out 
                  [fresh-f-s (-> Var? 
                                 (-> sub? any/c)
                                 (-> sub? any/c)
                                 )]))

        (provide (contract-out [struct Var ( (Name string?) (line number?) )]))
        (provide RVal?)

        ; -------------------------------- ;
)
