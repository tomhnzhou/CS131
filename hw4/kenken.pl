% True if E is the number in the table at coordinate Cord
square(Cord, T, E):- Cord = R-C, nth(R, T, Row), nth(C, Row, E). 

square_sum([], _, 0).
square_sum([Hcord|Tcord], T, Sum):- square(Hcord, T, E), square_sum(Tcord, T, Sum2), Sum #= Sum2 + E.

square_product([], _, 1).
square_product([Hcord|Tcord], T, Product):-  square(Hcord, T, E),  
											square_product(Tcord, T, Product2), Product #= Product2 * E. 

square_diff(Cord1, Cord2, T, Diff):- square(Cord1, T, E1), square(Cord2, T, E2),
									(Diff #= E1 - E2 ; Diff #= E2 - E1).

square_quot(Cord1, Cord2, T, Quot):- square(Cord1, T, E1), square(Cord2, T, E2),
									(E1 #= Quot * E2; E2 #= Quot * E1).

constraint(T, C):- C = +(Sum, L),  square_sum(L, T, Sum).
constraint(T, C):- C = *(Product, L), square_product(L, T, Product).
constraint(T, C):- C = -(Diff, Cord1, Cord2), square_diff(Cord1, Cord2, T, Diff).
constraint(T, C):- C = /(Quot, Cord1, Cord2), square_quot(Cord1, Cord2, T, Quot).

% =======================

%	kenken/3

%	For a 6*6 table, user time ~= 0.012 sec

% ========================

% Transpose of a table
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

% True if a list L of length N is legal
% (contains each number from 1 to N exactly once)
legal_rowcol(N, L):- length(L, N), fd_domain(L, 1, N), fd_all_different(L).

% True if the all rows and columns of an NxN table T is legal
legal_table(N, T):- 
		length(T, N), 
		maplist(legal_rowcol(N), T), 
		transpose(T, TT),
		maplist(legal_rowcol(N), TT).

kenken(N, C, T):- legal_table(N, T), maplist(constraint(T), C), maplist(fd_labeling, T).

% ========================

%	plain_kenken/3

%	For a 4*4 table, user time ~= 1 sec for all solutions

% ========================

% sample_rowcol(N, L).
% True if L is a sample legal row or col of length N
% e.g. N = 6, L = [6,5,4,3,2,1]
sample_rowcol(0, []).
sample_rowcol(N, [N|T]):- N > 0, M is N-1, sample_rowcol(M, T).

% True if L is a legal row or column
plain_legal_rowcol(N, L):- sample_rowcol(N, PL), permutation(PL, L).

% legal_rows(T, Itr, N)
% True if for all i <= Itr, the i-th row of an NxN table T is legal
legal_rows([], 0, _).
legal_rows([HT|TT], Itr, N):- Itr > 0, M is Itr-1, plain_legal_rowcol(N, HT), legal_rows(TT, M, N).
				
% True if all rows and columns of T are legal
plain_legal_table(T, N):- legal_rows(T, N, N), transpose(T, TT), legal_rows(TT,N,N).

% check_constraints(C, T)
% True if table T satisfy all constraints in C
check_constraints([], _).
check_constraints([Hc|Tc], T):- constraint(T, Hc), check_constraints(Tc, T).

plain_kenken(N, C, T):- 
		plain_legal_table(T, N),
		check_constraints(C, T).