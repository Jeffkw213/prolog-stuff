/** ---------------------------------------------------------

EECS 3401 Fall 2020 Assignment 2

Family name: Kwan

Given name: Jeff

Student number: 216 396 764



---------------------------------------------------------- */

/* load the three search algorithms */
:- ensure_loaded('astar.prolog').
:- ensure_loaded('astarCC.prolog').
:- ensure_loaded('idastar.prolog').

/* ------------------------------------------------------- */

/* successors( +State, -Neighbors)

   Neighbors is a list of elements (Cost, NewState) where
   NewState is a state reachable from State by one action and
   Cost is the cost for that corresponding action (=1 in our
   case)
*/
successors(S, NS) :-
   bagof(N, neighbour(S, N), NS).

neighbour(S, NS) :-
   (right(S, N);
   left(S,N);
   up(S,N);
   down(S,N)),
   NS=(1, N).



/* ------------------------------------------------------- */


/* equality(+S1, +S2)

   holds if and only S1 and S2 describe the same state
   if List S1 and S2 are the same
*/
equality([], []).
equality([H1|T1], [H2|T2]) :- 
   H1 = H2,
   equality(T1, T2).


/* ------------------------------------------------------- */

/* hfn_null( +State, -V)

   V is the null heuristic for State (=0 irrelevant of the state)
*/
hfn_null(_State, 0).



/* hfn_misplaced( +State, -V)

   V is the number of misplaced tiles in State
*/
hfn_misplaced(State, 0) :- goal(State),!.
hfn_misplaced(State, V) :- 
   length(State, V1), 
   numlist(1 , V1, L),
   compare_list(State, L, Num),
   V is Num - 1.



/* hfn_manhattan( +State, -V)

   V is the sum over the manhattan distances between the current
   and the designated position of each tile
*/
hfn_manhattan(State, 0) :- goal(State),!. 
hfn_manhattan(State, V) :- 
   build_goal(State, L),
   % location(State, X, Y, x),
   % location(L, Xa, Ya, x),
   % V is abs(X - Xa) + abs(Y - Ya).

   hfn_manhattan_calculation(State, L, 1, 0, V).

hfn_manhattan_calculation(State, Goal, Elem, InValue, InValue) :-
	same_length(State, Goal),
	length(State, Len),
	Elem =:= Len.

hfn_manhattan_calculation(State, Goal, Elem, InValue, Value):- 
   location(State, X, Y, Elem),
   location(Goal, Xa, Ya, Elem),
   V is InValue + abs(X - Xa) + abs(Y - Ya),
   NewElem is Elem + 1,
   hfn_manhattan_calculation(State, Goal, NewElem, V, Value).


/* ------------------------------------------------------- */


/* init( +Name, -State)
   x = null space
   State is the initial state for problem Name
*/
init(a,[1,2,3,
        4,8,5,
        x,7,6]).

init(b,[8,2,6,
        4,1,5,
        x,7,3]).

init(c,[x,2,6,
        4,1,5,
        8,7,3]).

init(d,[1,2,3,4,
        5,6,7,8,
        9,10,x,15,
        13,12,11,14]).


/* ------------------------------------------------------- */

/* goal( +State )

   holds if and only if State is a goal state
   when reaching ascending order and x is at the end of the list
   goal([1,2,3,4,8,5,x,7,6]).
   goal([1,2,3,4,5,6,7,8,9,10,x,15,13,12,11,14])
   goal([x,2,6,4,1,5,8,7,3])
   [1,2,3,4,5,6,7,8,x]
*/


goal(S) :- last(S,x), 
   select(x, S, T), solution(T).
solution(T) :- foreach((member(X, T), nth1(I, T, X)), (X is I)).


/* ------------------------------------------------------- */






/** ---------------------------------------------------------
   calling the search algorithms
  ---------------------------------------------------------- */

go(ProblemName, HFN) :-
	init(ProblemName, Init),
	astar(Init, successors, goal, HFN, Path, equality),
	writeln(Path).

goCC(ProblemName, HFN) :-
	init(ProblemName, Init),
	astarCC(Init, successors, goal, HFN, Path, equality),
	writeln(Path).

goIDA(ProblemName, HFN) :-
	init(ProblemName, Init),
	idastar(Init, successors, goal, HFN, Path, equality),
	writeln(Path).


/*
Helper predicates:
   - right (S, NS) -- move x tile right, S - current state, NS - new state
   - left (S, NS) -- move x tile left, S - current state, NS - new state
   - up (S, NS) -- move x tile up, S - current state, NS - new state
   - down (S, NS) -- move x tile down, S - current state, NS - new state
   - getnumberColumns(S, C), -- getting the number of columns of puzzle (sqrt(total number on list)), S - current state, C - number of columns
      getnumberColumns([1,2,3,4,8,5,x,7,6],C).
      C = 3.
   - getIndex(Index, X, Y) -- getting the element from the list by using X and Y coordinate, Index - position on list, X - the x coordinate, Y - y coordinate
      ie:
         1 2 3 
         4 5 6 
         7 8 x 

         getIndex(Index, 3, 2). 
         Index = 6.
   - location(State, X, Y, Ele) -- getting the X and Y coordinate of Ele, State - the state, X - x coordinate, Y - y coordinate, Ele - Element needed to find.
   - swap(State, I1, I2, Final) -- swapping the elements 
   - compare_list(List1, List2, V) -- comparing lists and return number of different


   - build_goal(State, Goal) -- creating a goal state with State, State- the state, Goal - the goal state.
   - add_last(List, Ele, NewList) -- inserting at the end of the list, List - list that needs element, Ele - Element that needs to put in List, NewList - the new List with the new Element
   
*/
/*Actions that can be made*/
right(State,Final) :- 
   location(State, X, Y, x),
   getnumberColumns(State, A),
   Num is A - 1,
	Y < Num,
	Z is Y + 1,
	getIndex(Index, X, Y, State),
	getIndex(NewIndex, X, Z, State),
	swap(State, Index, NewIndex, Final).

left(State,Final) :- 
	location(State, X, Y, x),
   Y > 0,
	Z is Y - 1,
	getIndex(Index, X, Y, State),
	getIndex(NewIndex, X, Z, State),
	swap(State, Index, NewIndex, Final).

up(State,Final) :- 
	location(State, X, Y, x),
   X > 0,
	Z is X - 1,
	getIndex(Index, X, Y, State),
	getIndex(NewIndex, Z, Y, State),
	swap(State, Index, NewIndex, Final).

down(State,Final) :- 
   location(State, X, Y, x),
   getnumberColumns(State, A),
   Num is A - 1,
   X < Num,
	Z is X + 1,
	getIndex(Index, X, Y, State),
	getIndex(NewIndex, Z, Y, State),
	swap(State, Index, NewIndex, Final).

/*
   Getting the location of the list.
----------------------------------------------------------------------
*/
location(State, X, Y, Elem) :-
	nth0(Index, State, Elem), getnumberColumns(State, Z),
	divmod(Index, Z, X, Y).

getIndex(Index, X, Y, S):- getnumberColumns(S, Num), Index is Num * X + Y.


getnumberColumns(S,C) :- list_length(S, Num), C is integer(sqrt(Num)).

list_length([], 0 ).
list_length([_|Xs] , L ) :- list_length(Xs,N) , L is N+1 .
/*
----------------------------------------------------------------------
*/

/*
   Swapping
----------------------------------------------------------------------
*/

swap(State, Index, NewIndex, Final):-
   same_length(State, Final),
   length(BeforeI1, Index),
	length(BeforeI2, NewIndex),
	append(BeforeI1, [EI1|PastI1], State),
	append(BeforeI1, [EI2|PastI1], Temp),
	append(BeforeI2, [EI2|PastI2], Temp),
	append(BeforeI2, [EI1|PastI2], Final).

/*
----------------------------------------------------------------------
*/

/*
   comparing two lists
----------------------------------------------------------------------
*/
compare_list([], [], 0).
compare_list([H1|T1], [H2|T2], C) :- 
   not(H1 = H2), !,
   compare_list(T1, T2, C1),
   C is C1 + 1.
compare_list([_|T1], [_|T2], C) :-
   compare_list(T1, T2, C). 
/*
----------------------------------------------------------------------
*/

/*
   Building a goal
*/
build_goal(S, V):- 
   length(S, L), 
   Num is L - 1,
   numlist(1, Num, Z),
   add_last(Z, x, V).

add_last([], Element, [Element]).
add_last([H|T], Element, [H|L]):- 
   add_last(T, Element, L).
/*
----------------------------------------------------------------------
*/