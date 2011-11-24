/* 
 * Student Name: Jonathan Neufeld
 * Student ID: 30671093
 * CS ID: p9d8
 * 
 * I'm not totally confident in my solutions, but I've literally been awake 37
 * hours and I don't think I can make this work any better. I tested everything
 * against the examples in the assignment pdf and it works, there's just some
 * slightly odd behavior sometimes, like in same_elements. Sorry!
 */

shuffle( [], [], [] ).
shuffle( [H | X], Y, [H | Z] ) :- shuffle( Y, X, Z ).

double( [], [] ).
double( [H | T], [H, H | R] ) :- double( T, R ). 

elem( [], _ ) :- false.
elem( [H | _], H ).
elem( [_ | T], E ) :- elem(T, E).

no_duplicates( [], [] ).
no_duplicates( [H | T], [H | R] ) :- not( elem( T, H ) ), no_duplicates( T, R ).
no_duplicates( [H | T], R ) :- elem( T, H ), no_duplicates( T, R ).

removal( E, [E | T], T ).
removal( E, [H | T], [H | R]) :- removal( E, T, R ).

same_elements( [], [] ).
same_elements( [H | T], R ) :- same_elements( T, A ), removal( H, R, A).

