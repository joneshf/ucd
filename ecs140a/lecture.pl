triple([], []).
triple([X|XS], [X1|YS]) :-
    X1 is X * 3,
    triple(XS, YS).

rtriple([], []).
rtriple([[]|Xs], [[]|Ys]) :-
    rtriple(Xs, Ys).
rtriple([[X|Xs]|Ys], [Nested|Rest]) :-
    rtriple([X|Xs], Nested),
    rtriple(Ys, Rest).
rtriple([X|XS], [X1|YS]) :-
    X1 is X * 3,
    rtriple(XS, YS).

%% This is zip.
pairs([], [], []).
pairs([X|Xs], [Y|Ys], [[X,Y]|Rest]) :-
    pairs(Xs, Ys, Rest).

funpairs([], [], []).
funpairs(Xs, Ys, Zs) :-
    member(X, Xs),
    member(Y, Ys),
    append([X])
