tower(N, T, C) :-
    % array size limits
    len_row(T, N),
    len_col(T, N),
    C = counts(Top, Bottom, Left, Right),
    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right, N),
    % finish domain limits
    within_domain_fd(T, N),
    maplist(fd_all_different, T),
    transpose(T, U),
    maplist(fd_all_different, U),
    maplist(fd_labeling, T),
    % check count on the each side
    % left
    left_count(T, Left),
    % right
    maplist(reverse, T, RT),
    left_count(RT, Right),
    % top
    left_count(U, Top), % U is the transpose of T
    % bottom
    maplist(reverse, U, RU),
    left_count(RU, Bottom).

% check there are N rows
len_row(T, N) :-
    length(T, N).

% check each of the rows has length N
len_col([], _).
len_col([HD | TL], N) :-
    length(HD, N),
    len_col(TL, N).

% check each of the integer is in the range [1, N]
within_domain_fd([], _).
within_domain_fd([HD | TL], N) :-
    fd_domain(HD, 1, N),
    within_domain_fd(TL, N).

% https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
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

% check left counts are correct
left_count([], []).
left_count([HD1 | TL1], [HD2 | TL2]) :-
    increasing(HD1, IS),
    length(IS, HD2),
    left_count(TL1, TL2).

% slightly modified from TA code
% record increasing sublist of a list
% design: 
%   increasing(List, Sublist)
%   increasing(List, Maxval, TmpSublist, ResultSublist)
increasing([HD | TL], IS) :-
    increasing(TL, HD, [HD], IS).
increasing([], _, Sublist, Sublist). % this is how we force "return"
increasing([HD | TL], Maxval, TmpSublist, IS) :-
    HD > Maxval,
    append(TmpSublist, [HD], NextSublist),
    increasing(TL, HD, NextSublist, IS).
increasing([HD | TL], Maxval, TmpSublist, IS) :-
    HD =< Maxval,
    increasing(TL, Maxval, TmpSublist, IS).

% acts like tower/3 but does not use finite domain solver
plain_tower(N, T, C) :-
    % array size limits
    len_row(T, N),
    len_col(T, N),
    C = counts(Top, Bottom, Left, Right),
    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right, N),
    % enumerate all possible integer solutions
    create_grid(T, N),
    % check count on the each side
    % left
    left_count(T, Left),
    % right
    maplist(reverse, T, RT),
    left_count(RT, Right),
    % top
    transpose(T, U),
    left_count(U, Top), % U is the transpose of T
    % bottom
    maplist(reverse, U, RU),
    left_count(RU, Bottom).

% a list contain N elements 
% http://www.gprolog.org/manual/html_node/gprolog033.html
% http://www.gprolog.org/manual/gprolog.html#hevea_default674
% Domain is all the enumerated answers of between(1, N, X)
within_domain_plain(N, Domain) :- 
    findall(X, between(1, N, X), Domain).

% fill in a 2D array with lists of fixed length (N)
% http://www.gprolog.org/manual/gprolog.html#sec215
fill_2d([], _, _).
fill_2d([Head | Tail], M1, N) :- % M is a matrix that has all different elements on its rows and cols
    within_domain_plain(N, Domain),
    permutation(Domain, Head),
    append(M1, [Head], M2),
    transpose(M2, MT),
    maplist(row_all_unique, MT),
    fill_2d(Tail, M2, N).

create_grid(Grid, N) :-
    length(Grid, N),
    fill_2d(Grid, [], N).

row_all_unique(Row) :-
    sort(Row, Sorted),
    length(Row, N_elements),
    length(Sorted, N_unique_elements),
    N_elements == N_unique_elements.

speedup(Ratio) :-
    statistics(cpu_time, [_, _]),
    plain_tower(4, _,
	  counts([2, 3, 3, 1],
		 [2, 1, 2, 4],
		 [2, 1, 3, 2],
		 [1, 2, 2, 3])),
    statistics(cpu_time, [_, PT]),
    tower(4, _,
	  counts([2, 3, 3, 1],
		 [2, 1, 2, 4],
		 [2, 1, 3, 2],
		 [1, 2, 2, 3])),
    statistics(cpu_time, [_, T]),
    Ratio is PT/T.

ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    \+ same_matrix(T1, T2).

same_matrix([], []).
same_matrix([HD1 | TL1], [HD2 | TL2]) :-
    maplist(=, HD1, HD2),
    same_matrix(TL1, TL2).
    
