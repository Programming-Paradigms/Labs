%% List primitives

%% length
mylength([], 0).
mylength([_|Rest], L2) :- mylength(Rest, L1), L2 is L1 + 1.

%% Prolog Intro
firstTwo(X, Y, [X,Y|_]).

contains(E, [E|_]).
contains(E, [_|Rest]) :- contains(E, Rest).

containsTest(E, [E|_]) :- !.
containsTest(E, [_|Rest]) :- contains(E, Rest), !.

notContains(E, L) :- \+ contains(E, L).

unique([], []).
unique([X|Rest], L1) :- unique(Rest, L1), containsTest(X, L1).
unique([X|Rest], L2) :- unique(Rest, L1), \+ containsTest(X, L1), L2 = [X|L1].

isList([_|_]).
isList([]).

listOnly([], []).
listOnly([X|Rest], [X|L1]) :- listOnly(Rest, L1), isList(X). 
listOnly([X|Rest], L1) :- listOnly(Rest, L1), \+ isList(X).

insertToSorted(E, [], [E]).
insertToSorted(E1, [E2|Rest], [E1, E2|Rest]) :- E1 =< E2, !.
insertToSorted(E1, [E2|Rest], [E2|R]) :- \+ (E1 =< E2), insertToSorted(E1, Rest, R), !.

insertSort([],[]).
insertSort([E|Rest], R) :- insertSort(Rest, RAux), insertToSorted(E, RAux, R), !.

%% A tree is a list [Root, Subtree1, Subtree2, ..., Subtreen]
size([_], 1) :- !.
size([Root, Child|OtherChildren], S) :-
	size([Root|OtherChildren], SAux1),
	size(Child, SAux2),
	S is SAux1 + SAux2, !.

%% Flatten is a slightly different size :).

eval(value(V), V).
eval(add(A, B), R) :- eval(A, R1), eval(B, R2), R is R1 + R2.
eval(mul(A, B), R) :- eval(A, R1), eval(B, R2), R is R1 * R2.
eval(var(X), R) :- asgn(X, E), eval(E, R).

asgn(a, value(2)).
asgn(b, add(var(a), value(3))).

test :-
	eval(add(value(1), mul(value(2), value(3))), R), display(R)
	,eval(add(var(a), mul(var(b), value(3))), R2), display(R2)
	.
