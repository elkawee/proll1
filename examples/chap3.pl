
sqlist([],[]).
sqlist([X|Xs],[Y|Ys]):- Y is X * X , sqlist(Xs,Ys).

%------------------------%

setify([],[]).
setify([X|T],L):- member(X,T),setify(T,L).
setify([X|T],[X|L]):- setify(T,L).

%------------------------%

reduce([X|L],X,L).
reduce([H|T],X,[H|L]):-reduce(T,X,L).


