:- [tests].
:- use_module(library(clpfd)).
:- set_prolog_flag(clpfd_monotonic, false).

snake(RowClues, ColClues, Grid, Solution)
:- copyGrid(Grid,Solution)
% , checkRowClues(Solution,RowClues)
% , checkColClues(Solution,ColClues)
, nonTouching(Solution) % snake cannot touch itself
, extend_grid(Solution,Extended_Solution)
, countNeighbors(Extended_Solution) % heads have 1 neighbor, midpoints 2
% , snakeConnected(Solution) % snake must be connected
.



%Potentialy this could never be called so it can be removed
nonTouching([]).

nonTouching([A,B]):-check_diagonal_neighbors(A,B).

nonTouching([A,B|Rest]) :- 
    check_diagonal_neighbors(A,B),
    nonTouching([B|Rest]).

check_diagonal_neighbors([A1,A2],[B1,B2]) :-  
    check_diagonal_pattern(A1,A2,B1,B2).   

check_diagonal_neighbors([A1,A2|RowA],[B1,B2|Rest]) :- 
    check_diagonal_pattern(A1,A2,B1,B2),
    check_diagonal_neighbors([A2|RowA],[B2|Rest]).

check_diagonal_neighbors([A1,A2],[B1,B2]) :- 
        check_diagonal_pattern(A1,A2,B1,B2).

check_diagonal_pattern(0,2,1,0) :- !,fail.
check_diagonal_pattern(0,1,2,0) :- !,fail.
check_diagonal_pattern(1,0,0,2) :- !,fail.
check_diagonal_pattern(2,0,0,1) :- !,fail.
check_diagonal_pattern(2,0,0,2) :- !,fail.
check_diagonal_pattern(0,2,2,0) :- !,fail.
check_diagonal_pattern(2,2,2,2) :- !,fail.
check_diagonal_pattern(_,_,_,_).



countNeighbors([]).
countNeighbors([A,B,C]) :- check_neighbors_rows(A,B,C).
countNeighbors([A,B,C|[D|Rest]]) :-
     check_neighbors_rows(A,B,C),
     countNeighbors([B,C,D|Rest]). 

check_neighbors_rows([_,A2],[B1,B2],[_,C2]) :-
    check_neighbors_pattern(B2,A2,0,C2,B1,Sum)
    .
    
check_neighbors_rows([_,A2],[B1,B2],[]) :-
    check_neighbors_pattern(B2,A2,0,0,B1,Sum)
    .


check_neighbors_rows([_,N,A3|RowA],[W,M,E|RowB],[_,S,C3|RowC]) :-
    check_neighbors_pattern(M,N,E,S,W,Sum),
    check_neighbors_rows([N,A3|RowA],[M,E|RowB],[S,C3|RowC]).

check_neighbors_rows([_,N,A3|RowA],[W,M,E|RowB],[]) :-
    check_neighbors_pattern(M,N,E,0,W,Sum),
    check_neighbors_rows([N,A3|RowA],[M,E|RowB],[]).
    

check_neighbors_pattern(0,_,_,_,_,_).
check_neighbors_pattern(-1,_,_,_,_,_).

check_neighbors_pattern(Piece,N,E,S,W,Sum) :- 1#=< Piece, 
    count_cell(N,X1),
    count_cell(E,X2),
    count_cell(S,X3),
    count_cell(W,X4),
    Sum #= X1+X2+X3+X4,
    print_cell(Piece),
    write(' == '),
    write(Sum),
    check_rules(Piece,Sum),
    nl.
    
count_cell(Point, 1) :- Point\=0.
count_cell(Point, 0) :- Point=0.


check_rules(1,1).
check_rules(2,2).

extend_grid(OldGrid,NewGrid) :-
    transpose(OldGrid,TransGrid),
    extend_grid_rows(TransGrid,RowTransGrid),
    transpose(RowTransGrid,RowGrid),
    extend_grid_rows(RowGrid,NewGrid).

extend_grid_rows([],[]).
extend_grid_rows([OldRow|OldRest],[NewRow|NewRest]) :-
     extend_row(OldRow,NewRow),
     extend_grid_rows(OldRest,NewRest).

% Extend a row by adding a 0 at both ends
extend_row(OldRow,NewRow) :- append([0|OldRow],[0],NewRow).

copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).

copyRow([],[]).
copyRow([-1|R],[_|S]) :- copyRow(R,S).
copyRow([Clue|R],[Clue|S]) :- copyRow(R,S).