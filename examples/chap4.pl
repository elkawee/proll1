membercheck(X ,[X|_]):-!.
membercheck(X ,[_|L]):- membercheck(X,L).


drink(milk).
drink(beer):-!.
drink(gin).

max(X,Y,X):-X>=Y,!.
max(X,Y,Y).
