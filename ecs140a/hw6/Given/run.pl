% used with the scripts for testing in "batch mode"

:- initialization(all).

all :- argument_value(1,X), call(X).
