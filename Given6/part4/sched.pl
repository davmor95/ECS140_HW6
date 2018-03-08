%%%%%%%% Part 1 %%%%%%%%%
%query national park names
np_names(N) :-
	np(N,_,_).

%qury national park names that are not yosemite
np_names_not_yosemite(N) :- 
	np(N,_,_), 
	N \= yosemite.

%query activities from yosemite national park
np_activities_yosemite(A) :-
	np(yosemite,_,A).

%query states associated with yosemite national park
np_states_yosemite(S) :-
	np(yosemite,S,_).

%query states associated with grandcanyon national park
np_states_grandcanyon(S) :-
 	np(grandcanyon,S,_).

%list states with national park name
np_states(N, S) :-
 	np(N,S,_).

%lit sorted activities from yosemite
np_sorted_activities_yosemite(A) :-
	np_activities_yosemite(A),
	sort(A).

%list all national parks that are exactly within one state
%State = [accept anything as head | nothing as tail]
np_single_state(N) :-
	np(N,S,_),
	S = [_|[]].

%list all national parks that are within two or more states
%State = [anything as head | tail] , tail = [not empty]
np_multi_state(N) :-
	np(N,S,_),
	S = [_ | T],
	T \= [].

%list pair of national parks that are exactly within one state of each other
% N is passed in as a list, split it to get N1 and N2 list, national parks
%H @< T compare national park names to see if they don't match each other (don't want repeats)
np_pair_names(N) :-
	[H|[T]] = N,
	np(H, S1, _),
	np(T, S2, _),
	S1 = S2,
	H @< T.

%list of parks within exactly 2 states and offer exactly 2 activities 
np_2_state_2_activities(N) :-
	np(N, S, A),
	S = [_| T1],
	T1 = [_| _],
	A = [_| T2],
	T2 = [_| _].

%names of parks that are within exactly one or exactly two states with or
np_12_states_1or(N) :-
	np(N, S, _),
	S = [_|[]];
	np(N, S, _),
	S = [_| T],
	T = [_| []].

%names of parks that are within exactly one or exactly two states without or
%do this by overloading functions
np_12_states_2wo(N) :-
	np(N, S, _),
	S = [_|[]].

np_12_states_2wo(N) :-
	np(N, S, _),
	S = [_| T],
	T = [_| []].
	

%names of parks that provide exactly camping and hiking (in either order); do this query three ways
%by defining only 1 rule in your code that uses Prolog’s or operator ";"
np_camping_hiking_1or(N) :-
	np(N, _, A),
	[H|T] = A,
	H = hiking,
	T = [camping | []];
	np(N, _, A),
	[H|T] = A,
	H = camping,
	T = [hiking | []].

%by defining only 2 rules in your code without using Prolog’s or operator ";"
%do this by overloading function
np_camping_hiking_2wo(N) :-
	np(N, _, A),
	[H|T] = A,
	H = hiking,
	T = [camping | []].

np_camping_hiking_2wo(N) :-
	np(N, _, A),
	[H|T] = A,
	H = camping,
	T = [hiking | []].

%by using sort
np_camping_hiking_sort(N) :-
	np(N, _, A),
	sort(A),
	A = [camping| [hiking]].

%%%%%%%%%% Part 2 %%%%%%%%%%%
%returns in list Z all elements of L and element E in sorted order
insert(L,E,Z) :-
	append(L,[E],Z),
	sort(Z).

%returns in list Z all elements of L, except for the last element of L
%if only 1 element in list return [], if empty list return nothing
butlast(L,Z) :-
	L \= [],
	append(Z, [_], L).

%Givenalist L, it returns in list NAL all the non-atoms from L and in list
%AL all the atoms from L. naaa preserves order: elements in NAL and AL appear in the same order as they did in L
%base case, if L is empty list, return empty list for AL and NAL
naaa([],[],[]).

naaa([H|T],NAL, AL) :- 
	atom(H),
	naaa(T,NAL,Temp),
	AL = [H|Temp].

naaa([H|T], NAL, AL) :-
	\+ atom(H),
	naaa(T,Temp,AL),
	NAL = [H|Temp].

%%%%%%%%%%% Part 3 %%%%%%%%%%%
%if X (head) does not equal pivot, recurse 
splitlist([X|Remain], Left, Pivot, Right) :-
	X \== Pivot,
	splitlist(Remain, Y, Pivot, Right),
	Left = [X|Y].

%if X (head) equals pivot, call helper function then back out
splitlist([X|Remain], Left, Pivot, Right) :-
	X == Pivot,
	splitlistR(Remain, Left, Pivot, Right).

%helper function for splitlist, once found pivot, initialize Right as remainder of list
splitlistR(L, [], _, Right) :-
	Right = L.

%if H is not Owner, recurse
split3list([H|Remain], Owner, Left, Pivot, Right) :-
	\+ check3rd(H, Owner),
	split3list(Remain, Owner, Temp, Pivot, Right),
	Left = [H|Temp].

%if H is Owner, call helper function
split3list([H|Remain], Owner, Left, Pivot, Right) :-
	check3rd(H, Owner),
	Pivot = H,
	split3listR(Remain, Owner, Left, Pivot, Right).

%helper function, when Owner is found, initialize Right as list
split3listR(L, _, [], _, Right) :-
	Right = L.

%helper function checks if third element (index 2) is element of L list(tuple)
%returns true if H is owner, else false
check3rd(L, Owner) :-
	nth0(2, L, H),
	Owner == H.

%%%%%%%%Part4%%%%%%%%%%%
%Base Case, given an empty list, return an empty list.
perm([],[]). 

perm(L,PermL) :-
	select(E1,L,R),		 %Call select to backtrack on all the element of L
	perm(R,Temp), 			 %Recurse on R which is L without E1.
	PermL = [E1|Temp].		 %Store E1 as well as whatever permL returns (Temp).

permsub(L,PermL) :-
	naaa(L, NAL1, _),		 %call naaa to get the non-atoms list of the original L.
	perm(L, R), 		 %call perm to get the permuation, permuation stored in R.
	naaa(R, NAL2, _),		 %call naaa to get the non-atoms of R.
	NAL1 == NAL2, 			 %check non-atoms list from L to non-atoms list of R.
	PermL = R.			 % store in PermL if check is true.









	
	
