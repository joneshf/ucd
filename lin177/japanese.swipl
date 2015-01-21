japanese([adamu],        [nounphrase],         [adam],    [entity]).
japanese([iivu],         [nounphrase],         [eve],     [entity]).
japanese([waraimasu],    [verb, intransitive], [laughs],  [property]).
japanese([nakimasu],     [verb, intransitive], [cries],   [property]).
japanese([mimasu],       [verb, transitive],   [watches], [relation]).
japanese([tetsudaimasu], [verb, transitive],   [helps],   [relation]).
japanese(VerbPhrase,     [verbphrase],         Property,  [property]) :-
    japanese(VerbPhrase, [verb, intransitive], Property, [property]).
japanese(VerbPhrase,     [verbphrase],         Property,  [property]) :-
    japanese(TransitiveVerb, [verb, transitive], Relation, [relation]),
    japanese(AccusativeNounPhrase, [nounphrase, accusative], Entity, [entity]),
    append(AccusativeNounPhrase, TransitiveVerb, VerbPhrase),
    append(Entity, Relation, Property).
japanese(AccusativeNounPhrase, [nounphrase, accusative], Entity, [entity]) :-
    japanese(NounPhrase, [nounphrase], Entity, [entity]),
    append(NounPhrase, [o], AccusativeNounPhrase).
japanese(Sentence, [sentence], Proposition, [proposition]) :-
    japanese(NominativeNounPhrase, [nounphrase], Entity, [entity]),
    japanese(VerbPhrase, [verbphrase], Property, [property]),
    reverse(CorrectProperty, Property),
    append(NominativeNounPhrase, [ga|VerbPhrase], Sentence),
    append(Entity, CorrectProperty, Proposition).
