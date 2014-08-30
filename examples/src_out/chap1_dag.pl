a(g,h).
a(g,d).
a(e,d).
a(h,f).
a(e,f).
a(a,e).
a(a,b).
a(b,f).
a(b,c).
a(f,c).


path(X,X).
path(X,Y) :- a(X,Z),path(Z,Y).

go :- findall( (X) , path(g,X) , L ) , print(L),
      format('~n-----------------~n'),
      findall( (X) , path(X,c) , L2 ) , print(L2).
