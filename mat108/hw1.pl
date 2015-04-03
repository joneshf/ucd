truth('T', true).
truth('F', false).

and(X, Y) :-
    X,
    Y.

or(X, Y) :-
    X ; Y.

eval(P, truth('T', true)) :-
    P,
    !.
eval(_, truth('F', false)).

dist(P, Q, R) :-
    truth(P, Pval),
    write(P),
    write(' & '),
    truth(Q, Qval),
    write(Q),
    write(' & '),
    truth(R, Rval),
    write(R),
    write(' & '),
    eval(or(Qval, Rval), truth(QandR, QandRval)),
    write(QandR),
    write(' & '),
    eval(and(Pval, QandRval), truth(PorQandR, _PorQandRval)),
    write(PorQandR),
    write(' & '),
    eval(and(Pval, Qval), truth(PandQ, PandQval)),
    write(PandQ),
    write(' & '),
    eval(and(Pval, Rval), truth(PandR, PandRval)),
    write(PandR),
    write(' & '),
    eval(or(PandQval, PandRval), truth(PandQorPandR, _PandQorPandRval)),
    write(PandQorPandR),
    write(' \\\\ '),
    nl,
    fail.

interchange(P, Q, R) :-
    truth(P, Pval),
    truth(Q, Qval),
    truth(R, Rval),
    write(P),
    write(' & '),
    write(Q),
    write(' & '),
    write(R),
    write(' & '),
    eval(and(Pval, Qval), truth(PandQ, PandQval)),
    write(PandQ),
    write(' & '),
    eval(or(PandQval, Rval), truth(PandQorR, _PandQorRval)),
    write(PandQorR),
    write(' & '),
    eval(and(Qval, Rval), truth(QandR, QandRval)),
    write(QandR),
    write(' & '),
    eval(or(Pval, QandRval), truth(PorQandR, _PorQandRval)),
    write(PorQandR),
    write(' \\\\ '),
    nl,
    fail.

main :-
    %% dist(_A, _B, _C),
    interchange(_D, _E, _F).
