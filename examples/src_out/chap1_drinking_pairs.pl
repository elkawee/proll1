
drinks(john, martini).
drinks(mary, gin).
drinks(susan,wodka).
drinks(john, gin).
drinks(fred, gin).

drinks(james-b, martini).

pair(X,Y) :- drinks(X,Z),drinks(Y,Z),X\==Y.

go :- findall( (A,B) , pair(A,B) , L ) , print(L).
