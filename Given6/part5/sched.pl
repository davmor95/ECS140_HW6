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

perm([],[]).

perm(L,PermL) :-
	select(E1,L,R),
	perm(R,Y),
	PermL = [E1|Y].

permsub(L,PermL) :-
	naaa(L, Y2, _),
	perm(L,R),
	naaa(R, Y4, _),
	Y2 == Y4,
	PermL = R.

fit1stRequest([Owner, Size], MemList, NewMemList) :-
    split3list(MemList, z, L1, P1, R1),
    nth0(0,P1,X),
    nth0(1,P1,Y),
    Y >= Size,
    add(X,Size,RemStart),
    sub(Y,Size,RemSize),
    RemSize > 0,
    append(L1, [[X,Size,Owner]], Temp1),
    append(Temp1, [[RemStart, RemSize, z]], Temp2),
    append(Temp2, R1, NewMemList).


fit1stRequest([Owner, Size], MemList, NewMemList) :-
    split3list(MemList, z, L1, P1, R1),
    nth0(0,P1,X),
    nth0(1,P1,Y),
    Y >= Size,
    sub(Y,Size,RemSize),
    RemSize =< 0,
    append(L1, [[X,Size,Owner]], Temp1),
    append(Temp1, R1, NewMemList).

add(X, Y, Sum) :-
    Sum is (X+Y).

sub(X, Y, Sum) :-
    Sum is (X-Y).



