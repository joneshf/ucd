sublist([ ], List).
sublist([Item | Tail], List) :-
    member(Item, List),
    sublist(Tail, List).
