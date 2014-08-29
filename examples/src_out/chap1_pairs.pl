
male(bert).
male(parcival).

female(lucinda).
female(camilla).

pair(X,Y) :- male(X),female(Y).
pair(X,Y) :- female(X),male(Y).


go :- findall( (A,B) , pair(A,B) , L ) , print(L).