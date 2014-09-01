
(module var_and_sub racket 

	; supertype for all structures, that make up the interpreter - universes must not bleed into each other 
        (struct proll-type ()) 
	; 
        (struct Var proll-type (Name line)
		#:methods gen:custom-write [(define (write-proc V port w?) 
					      (fprintf port "~a:l~s" (Var-Name V) (Var-line V) ) ) ]
		)
        (struct empty proll-type ()
		#:methods gen:custom-write [(define (write-proc e port w? )
					      (fprintf port "|_|")   )]
		)
        (define (RVal? X) (not (proll-type? X )))
        

        
        (struct sub proll-type ())
        
        (struct empty-sub sub ()
		#:methods gen:custom-write [(define (write-proc e port w?)
					      (fprintf port "[<>]")   ) ] 
		)


	; elementary substitution X -> Y
	; where X must be a run-time-variable 
	; and Y Variable, <empty> or any payload term ( racket data, is not in one of the interpreter private types ) 

        (struct elem-sub proll-type (lhs rhs) 
                #:transparent 
                #:guard (lambda (lhs rhs t-name) 
                          (cond 
                           [(and (Var? lhs)
                                 (or (Var? rhs) (RVal? rhs) (empty? rhs)))
                            (values lhs rhs)]
                           [else (error (format "invalid arguments for elem-sub (%s,%s)" lhs rhs )) ]
			   ))
		#:methods gen:custom-write [(define (write-proc elemS port w? ) (match-let ( [(elem-sub lhs rhs) elemS])
										  (fprintf port "[~s->~s]"  lhs rhs))  )]
		)
	
	; essentially a list of elem-sub
	
        (struct sub-comp sub (e S)
                #:transparent
                #:guard (lambda (e S t-name )
                          (match* (e S) 
                                 [ ( (elem-sub _ _) (or (empty-sub) (sub-comp _ _ )) ) (values e S )]
                                 [ ( _ _ ) (error "") ]))
		#:methods gen:custom-write [(define (write-proc X port w? ) (match-let [((sub-comp A B) X ) ] 
									      (fprintf port "~s , ~s" A B )  ) )]
		)


        ; for a given left-hand-side return the first right hand side, that occurs in 
	; the substitution set ( sub-comp ) 

        (define (assoc  lhs S )
          (cond 
           [(empty-sub? S) 'fail ]
           [(eq? lhs (elem-sub-lhs (sub-comp-e S)))
            (elem-sub-rhs (sub-comp-e S))]
           [else (assoc lhs (sub-comp-S S))]
           ))

	; in case of the right hand side beeing a variable too - the lookup must be done anew 
	; Example ( X =:= Y ), ( Y=:=5), (query X ) 

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

          (define (ex L R S )                 ; extend a substitution S with (L->R)
            (sub-comp (elem-sub L R ) S ))
          (let ([X_ (walk X S)]
                [Y_ (walk Y S)] )
            (cond
             [(eq? X_ Y_) S]
             [else 
              (match* ( X_ Y_ )
                      [ ( (Var _ _) _ ) (ex X_ Y_ S ) ]
                      [ ( _ (Var _ _) ) (ex Y_ X_ S ) ] 
                      [ ( (cons Xh Xt) (cons Yh Yt))    ; cons cells as the only structural element unify can "look into" 
                       					; other data structures need to be explicitly supported here 
                        (match (unify Xh Yh S )
                               [ 'fail 'fail ]
                               [ (and S_ (sub)) (unify Xt Yt S_) ])]
                      [ ( _ _ ) (begin 
                                  (cond 
                                   [(equal? X_ Y_ ) S ]
                                   [else 'fail ]))])])))


        
        (define (query X S) ; TODO :: weiss auch nicht mehr woher diese Anmerkung kam : (macht nicht wirklich sinn weils nicht matchen kann ) 
          ;(printf "query for : ~s~n in ~s~n:" X S )
          (match X
                 [ (Var _ _) (assoc* X S) ]
                 [ (cons H T) (cons (query H S) (query T S )) ]
                 [ (or (? RVal?) (empty) ) X ]))

	(define (ext-s V X Sub)
	  (sub-comp (elem-sub V X ) Sub ))

	(provide ext-s)


                          

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

