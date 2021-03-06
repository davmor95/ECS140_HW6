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
np_12_states_2wo(N) :-
	np_12_states_1or(N).

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
np_camping_hiking_2wo(N) :-
	np_camping_hiking_1or(N).

%by using sort
np_camping_hiking_sort(N) :-
	np(N, _, A),
	sort(A),
	A = [camping| [hiking]].

%State = [accept anything as head | nothing as tail]
%S = [_|[]]
insert(L,E,Z) :-
	append(L,[E],Z),
	sort(Z).
butlast(L,Z) :-
	L \= [],
	append(Z, [_], L).

naaa(L,NAL,AL) :-
	atom(L),
	NAL = L,
	AL = L.
	

