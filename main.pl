piece(white, rook,   1, 1).
piece(white, knight, 2, 1).
piece(white, bishop, 3, 1).
piece(white, queen,  4, 1).
piece(white, king,   5, 1).
piece(white, bishop, 6, 1).
piece(white, knight, 7, 1).
piece(white, rook,   8, 1).
piece(white, pawn,   X, 2) :-
  between(1, 8, X).

piece(black, rook,   1, 8).
piece(black, knight, 2, 8).
piece(black, bishop, 3, 8).
piece(black, queen,  4, 8).
piece(black, king,   5, 8).
piece(black, bishop, 6, 8).
piece(black, knight, 7, 8).
piece(black, rook,   8, 8).
piece(black, pawn,   X, 7) :-
  between(1, 8, X).

initial_board(Board) :-
  findall(piece(A, B, C, D), piece(A, B, C, D), Board).

up(1, 2).
up(2, 3).
up(3, 4).
up(4, 5).
up(5, 6).
up(6, 7).
up(7, 8).

col_trans(a, 1).
col_trans(b, 2).
col_trans(c, 3).
col_trans(d, 4).
col_trans(e, 5).
col_trans(f, 6).
col_trans(g, 7).
col_trans(h, 8).

opposite(white, black).
opposite(black, white).


% CHECK IF THE (C, L) CELL HAS NO PIECE IN IT
empty(Board, C, L) :- 
  \+(member(piece(_, _, C, L), Board))
.

% CHECK IF A SPECIFIC PIECE IS ON THE BOARD AT A SPECIFIC LOCATION
exists(Type, Piece, Board, C, L) :-
  member(piece(Type, Piece, C, L), Board)
.

% REMOVE THE PIECE WHICH IS AT LOCATION (C, L)
% THIS SHOULD BE GUARDED BY A CAN MOVE PREDICATE
remove_piece(C, L, Before, After) :-
  delete(Before, piece(_, _, C, L), After)
.

% ADDS A PIESE OF A SPECIFIC TYPE AT A SPECIFIC LOCATION (C, L)
% THIS SHOULD BE GUARDED BY A CAN MOVE PREDICATE
add_piece(Turn, Piece, C, L, [], [piece(Turn, Piece, C, L)]).
add_piece(Turn, Piece, C, L, [H | T1], [H | T2]) :-
  add_piece(Turn, Piece, C, L, T1, T2)
.

%% THIS MOVES A PIECE FROM ONE LOCATION TO ANOTHER
%% THIS SHOULD BE GUARDED BY A CAN MOVE PREDICATE
move_piece(Turn, Piece, A, B, X, Y, Before, After) :- 
  remove_piece(A, B, Before, Middle),
  add_piece(Turn, Piece, X, Y, Middle, After)
.

% PREDICATES TO CHECK IF WE CAN MOVE A PAWN TO A SPECIFIC LOCATION
% THERE ARE 2 TRIES FOR SIMPLE MOVES: ONE TO HANDLE ONE STEP FORWARD AND 
% ONE TO HANDLE TWO STEPS FORWARD IF IT WASN'T MOVED UNTIL NOW 
can_move(white, pawn, C, L, Board, C, FROML, 1) :-
  up(FROML, L),
  empty(Board, C, L),
  exists(white, pawn, Board, C, FROML)
.

can_move(white, pawn, C, 4, Board, C, 2, 2) :-
  empty(Board, C, 3),
  empty(Board, C, 4),
  exists(white, pawn, Board, C, 2)
.

can_move(black, pawn, C, L, Board, C, FROML, 1) :-
  up(L, FROML),
  empty(Board, C, L),
  exists(black, pawn, Board, C, FROML)
.

can_move(black, pawn, C, 5, Board, C, 7, 2) :-
  empty(Board, C, 6),
  empty(Board, C, 5),
  exists(black, pawn, Board, C, 7)
.

% MOVE A BISHOP
make_move([bishop | _], Before, Before, Turn) :-
  write(ok),
  write(Turn)
.

% MOVE A PAWN
make_move([C, L], Before, After, Turn) :-
  col_trans(C, NC),
  can_move(Turn, pawn, NC, L, Before, X, Y, _),
  move_piece(Turn, pawn, X, Y, NC, L, Before, After)
.

% ITERATE THE LIST OF MOVES
make_moves([], Before, Before, _).
make_moves([H|T], Before, After, Turn) :-
  make_move(H, Before, Middle, Turn),
  opposite(Turn, Next),
  make_moves(T, Middle, After, Next)
.

% ENTRYPOINT AND INPUT FILE PARSING
main :-
    open('input.txt', read, Str),
    read_file(Str,Lines),
    close(Str),
    initial_board(Board),
    make_moves(Lines, Board, Result, white),
    write(Result)
.

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).