listOfPairs(_, [], []).
listOfPairs(FirstElement, [X|XS], All) :-
	listOfPairs(FirstElement, XS, WithoutXsPair),
	All = [[FirstElement, X]|WithoutXsPair].

cartesian([], _, []).
cartesian([X|XS], OtherSet, Product) :-
	cartesian(XS, OtherSet, PairsWithoutX),
	listOfPairs(X, OtherSet, PairsWithX),
	append(PairsWithoutX, PairsWithX, Product).

union([], Y, Y).
union([X|XS], Y, Union) :-
	member(X, Y),
	union(XS, Y, Union).
union([X|XS], Y, Union) :-
	\+(member(X, Y)),
	union(XS, Y, XSandYUnion),
	Union = [X|XSandYUnion].

intersect([], _, []).
intersect([X|XS], Y, Intersect) :-
	\+(member(X, Y)),
	intersect(XS, Y, Intersect).
intersect([X|XS], Y, Intersect) :-
	member(X, Y),
	intersect(XS, Y, XSandYIntersection),
	Intersect = [X|XSandYIntersection].

diff([], _, []).
diff([X|XS], Y, Diff) :-
	member(X, Y),
	diff(XS, Y, Diff).
diff([X|XS], Y, Diff) :-
	\+(member(X, Y)),
	diff(XS, Y, XSandYDiff),
	Diff = [X|XSandYDiff].

%% Notice how union, intersect and diff are related.

insertToAll(_, [], []).
insertToAll(Element, [X|XS], All) :-
	insertToAll(Element, XS, Interm),
	All = [[Element|X]|Interm].

pow([], [[]]).
pow([X|XS], Pow) :-
	pow(XS, IntermPow),
	insertToAll(X, IntermPow, IntermPowWithX),
	append(IntermPow, IntermPowWithX, Pow).

putEverywhere(X,Destination,[X|Destination]).
putEverywhere(X,[H|Destination],[H|WithX]) :- putEverywhere(X,Destination,WithX).

perm([],[]).
perm([H|T],L) :-
	perm(T,U),
	putEverywhere(H,U,L).

%% Ar and comb will be left to reader's enjoyment.