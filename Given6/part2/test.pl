test(N,R) :- R is N*N.

?- map([3,5,-2], test, L).
L = [9,25,4] ;