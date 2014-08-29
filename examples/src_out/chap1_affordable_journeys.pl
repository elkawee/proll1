border(sussex, kent).
border(sussex, surry).
border(surrey, kent).
border(hampshire, sussex).
border(hampshire, surrey).
border(hampshire, berkshire).
border(berkshire, surrey).
border(wiltshire, hampshire).
border(wiltshire, berkshire).


adjacent(X, Y) :- border(X, Y).
adjacent(X, Y) :- border(Y, X).

affordable(X, Y) :- adjacent(X,Z),adjacent(Z,Y),X\==Y.

go :- findall( (X,Y) , affordable(X,Y) , L ) , print(L).


