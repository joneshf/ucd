t1([H],H).
t1([H|T],Z) :-
    t1(T,A),
    ( Z=A ; Z=H ).

t2([H],H).
t2([H|T],Z) :-
    t2(T,A),
    ( (Z=A, !) ; Z=H ).

t3([H],H).
t3([H|T],Z) :-
    t2(T,A),
    ( Z=A ; (Z=H, !) ).

t4([],[]).
t4(L,Z) :-
    select(E,L,R),
    t4(R,W),
    append(W,[E],Z).
/*adasda*/

t1a([],[]).
t1a(L,Z) :-
    select(E,L,R),
    t1a(R,X), /* diff from t1 */
    X=W, /* diff from t1 */
    append(W,[E],Z).

t1b([],[]).
t1b(L,Z) :-
    select(E,L,R),
    t1b(R,X), /* diff from t1 */
    W=X, /* diff from t1 */
    append(W,[E],Z).

t1c([],[]).
t1c(L,Z) :-
    select(E,L,R),
    t1c(R,W),
    Z=[W|[E]]. /* diff from t1 */

t5([],[]).
t5(L,Z) :-
    select(E,L,R),
    t5(R,W),
    ((W = [H|_], E < H) ; W = []),
    append([E],W,Z).

ct(N, I) :-
    course(N, _, I).
    %% member(I, Instructors).
