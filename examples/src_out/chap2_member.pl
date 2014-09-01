member(X,[X|_]).
member(X,[_|T]):- member(X,T).

mystery(X, L1, L2):- member(X, L1), member(X, L2).

go :- L1 = [ a , b , c ,d ] ,
      L2 = [ b , a],
      findall( X , mystery( X , L1 , L2 ) , Res ) ,
      print( Res ) . 
