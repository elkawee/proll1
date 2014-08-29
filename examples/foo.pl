inner([],[],R ) :- R is 0.
inner([Ha|Ta], [Hb|Tb], R) :- inner(Ta, Tb, R1), R is (Ha * Hb) + R1 . 

inner2(V1, V2, R):- innerd(V1,V2,0,R).

innerd([],[],Acc,Acc).
innerd([A|As], [B|Bs], Acc, R) :- Acc1 is A*B + Acc , innerd(As,Bs,Acc1,R).

%----------------------------------

maxbin(A,B,A):-A>B.
maxbin(A,B,B):-B>=A.

max([X],X).
max([C|Cs],R):- max(Cs,H), maxbin(C,H,R).

minbin(A,B,A):-A < B.
minbin(A,B,B):-B =< A.

min([X],X).
min([C|Cs],R):- min(Cs,H), minbin(C,H,R).

minmax([X],X,X).
minmax([H|Hs],Min,Max):- minmax(Hs, Min1 , Max1) , minbin(H,Min1,Min) , maxbin(H,Max1,Max).

%----------------------------------


a(g,h).
a(d,a).
a(g,d).
a(e,d).
a(h,f).
a(e,f).
a(a,e).
a(a,b).
a(b,f).
a(b,c).
a(f,c).

path(A,B):-path1(A,B,[]).

nonmember(_,[]).
nonmember(A,[B|Bs]):-A\==B,nonmember(A,Bs).

path1(A,B,R,R):-a(A,B).
path1(A,B,T,R):-a(A,X),nonmember(X,T),path1(X,B,[X|T],R).
