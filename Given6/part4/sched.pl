np_names(N) :- 
	np(N,_,_).

np_names_not_yosemite(N) :- 
	np(N,_,_), 
	N \= yosemite.

np_activities_yosemite(A) :-
	np(yosemite,_,A).

np_states_yosemite(S) :-
	np(yosemite,S,_).

np_states_grandcanyon(S) :- 
	np(grandcanyon,S,_).

np_states(N,S) :-
	np(N,S,_).

np_sorted_activities_yosemite(A) :-
	np_activities_yosemite(A),
	sort(A).

np_single_state(N) :-
	np(N,S,_),
	S = [_|[]].

np_multi_state(N) :-
	np(N,S,_),
	S = [_,_|_].
	
np_pair_names(N) :-
	[H|[T]] = N,
	np(H, S1, _),
	np(T, S2, _),
	S1 = S2,
	H @< T.

np_2_state_2_activities(N) :-
	np(N,S,A),
	S = [_,_|[]],
	A = [_,_|[]].

np_12_states_1or(N) :-
	np(N, S, _),
	S = [_|[]];
	np(N, S, _),
	S = [_,_|[]].

np_12_states_2wo(N) :-
	np(N, S, _),
	S = [_|[]].

np_12_states_2wo(N) :-
	np(N, S, _),
	S = [_,_|[]].

np_camping_hiking_1or(N) :-
	np(N,_,A),
	A = [A1,A2|[]],
	A1 = camping,
	A2 = hiking;
	np(N,_,A),
	A = [A1,A2|[]],
	A1 = hiking,
	A2 = camping.


np_camping_hiking_2wo(N) :-
		np(N,_,A),
	A = [A1,A2|[]],
	A1 = camping,
	A2 = hiking.


np_camping_hiking_2wo(N) :-
	np(N,_,A),
	A = [A1,A2|[]],
	A1 = hiking,
	A2 = camping.


np_camping_hiking_sort(N) :-
	np(N,_,A),
	sort(A),
	A = [camping, hiking].

%%%%%%%%%%%Part2%%%%%%%%%%%


insert(L,E,Z) :-
	append(L,[E], Z),
	sort(Z).

butlast([_|[]], []).

butlast([H|T], Z) :-
	butlast(T, Y),
	Z = [H|Y].

naaa([],[],[]).

naaa([E1|R],NAL, AL) :- 
	atom(E1),
	naaa(R,NAL,Y1),
	AL = [E1|Y1].

naaa([E1|R], NAL, AL) :-
	\+ atom(E1),
	naaa(R,Y1,AL),
	NAL = [E1|Y1].

%%%%%%%%%%%%%Part3%%%%%%%%%%%%%%%

splitlist([X|Remain], Left, Pivot, Right) :-
	X \== Pivot,
	splitlist(Remain, Y, Pivot, Right),
	Left = [X|Y].

splitlist([X|Remain], Left, Pivot, Right) :-
	X == Pivot,
	splitlistR(Remain, Left, Pivot, Right).

splitlistR(L, [],_, Right) :-
	Right = L.


split3list([X|Remain], Owner, Left, Pivot, Right) :-
	\+ check3rd(X, Owner),
	split3list(Remain, Owner, Y, Pivot, Right),
	Left = [X|Y].

split3list([X|Remain], Owner, Left, Pivot, Right) :-
	check3rd(X, Owner),
	Pivot = X,
	split3listR(Remain, Owner, Left, Pivot, Right).

split3listR(L, _, [], _, Right) :-
	Right = L.

check3rd(L, Owner) :-
	nth0(2, L, X),
	Owner == X.

%%%%%%%%Part4%%%%%%%%%%%

perm([],[]). %Base Case, given an empty list, return an empty list.

perm(L,PermL) :-
	select(E1,L,R), %Call select to backtrack on all the element of L
	perm(R,Y), %Recurse on R which is the L without E1.
	PermL = [E1|Y]. %Store E1 as well as permL returns (Y).

permsub(L,PermL) :-
	naaa(L, Y2, _), %call naaa to get the non-atoms list of the original L.
	perm(L, R), %call perm to get the permuation, permuation stored in R.
	naaa(R, Y4, _), %call naaa to get the non-atoms of R.
	Y2 == Y4, %check non-atoms list from L to non-atoms list of R.
	PermL = R. % store in PermL if check is true.