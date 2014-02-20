% run only on Linux PCs:
%
%     use only gprolog.  the command to use is simply ``gprolog''.
%
%     note: On CSIF, gprolog resides in /usr/bin;
%           gprolog assumes that you have that directory in your PATH.
%           unless you've done something truly weird, you already
%              have that in your PATH.
%           but, if you don't, then
%              in csh or tcsh:
%                 setenv PATH /usr/bin:$PATH
%              in sh, bash, or ksh:
%                 PATH=/usr/bin:$PATH; export PATH
%
%     (this code [should] also works with bp (BinProlog), but don't use that.)


% run all tests via just test.
% or you can run tests for individual parts,
% e.g., test_courses. or just tc. for short.

%
%
% be sure you have ``consulted'' all files you need to run a particular
% test or tests. And, of course, you'll need to ``consult'' this file too!
%
% the format of each test differs slightly from
% the equivalent interactive test, although it's straightforward to
% extract the interactive test if you want to test something
% individually, as you might during debugging.  roughly:
%  -- remove the ``all'' predicate and any string arguments (e.g., "X" or "Z").
%  -- add parentheses after predicate name.
%  -- put . at end instead of , (if necessary).
% E.g.,
%	all3iio( delete, [1], 1, Z, "Z")
% represents
%	delete( [1], 1, Z ).

% use these predicates to test everything and create Output.your
% then exit from gprolog.
% then compare the correct output with your output.
% e.g.,
%   start up gprolog, consult everything you need, and then type `ty1.'.
%   then exit from gprolog.
%   then:
%     diff Output.correct1 Output.your1
%
%   or:
%    tkdiff Output.correct1 Output.your1
%
% proceed similarly for the other parts.
%
% files should be identical except perhaps for your output for
%
%   (1)
%
%          c_12_inst_1or
%          c_12_inst_2wo
%
%       can differ from the "correct" output in order
%       and whether or not it has duplicates.
%       (Reason: there are a few ways to write this
%       predicate other than the way my solution does it.)
%
%   (2) the delete_question.
%
%   (3) the UNSORTED tests of the scheduling predicates.
%       for ONLY those parts, your overall answers should be the same,
%       but the order in which they are generated might differ
%       and you may or may not generate duplicates.

ty :-
	tell('Output.your'), test, told.
% each of these ty predicates tests everything upto and including that part
ty1 :-
	tell('Output.your1'), test_p1, told.
ty2 :-
	tell('Output.your2'), test_p1, test_p2, told.
ty3 :-
	tell('Output.your3'), test_p1, test_p2, test_p3, told.
ty4 :-  % nearly same as ty, except name of output file.
	tell('Output.your4'), test_p1, test_p2, test_p3, test_p4, told.
ty5 :-  % nearly same as ty, except name of output file.
	tell('Output.your5'),
	test_p1, test_p2, test_p3, test_p4, test_p5,
	told.

% as above, but for testing just individual subparts of part5.
ty5a :-
	tell('Output.your5a'), test_p5a, told.
ty5b :-
	tell('Output.your5b'), test_p5b, told.
ty5c :-
	tell('Output.your5c'), test_p5c, told.
ty5d :-
	tell('Output.your5d'), test_p5d, told.


% test everything.
% (you probably don't print out all the output -- it's too many pages.)
test :-
	test_p1,
	test_p2,
	test_p3,
	test_p4,
	test_p5.

test_p1 :-
	test_courses.
test_p2 :-
	test_lists.
test_p3 :-
	test_distribute.
test_p4 :-
	test_myfor,
	test_crossmyfor.
test_p5 :-
	test_getallmeetings,
	test_participants,
	test_osched,
	test_xsched.

test_p5a :-
	test_getallmeetings.
test_p5b :-
	test_participants.
test_p5c :-
	test_osched.
test_p5d :-
	test_xsched.

/* some aliases to save typing; see further aliases below for scheduling */
tc :-
	test_courses.
tl :-
	test_lists.
td :-
	test_distribute.
tf :-
	test_myfor.
tr :-
	test_crossmyfor.
tg :-
	test_getallmeetings.
tp :-
	test_participants.
to :-
	test_osched.
tx :-
	test_xsched.

/* define some facts for the first part.
 * note: these *approximate* some ECS courses and their offerings here.
 * prolog doesn't allow atom names like 140a and 140b;
 * could instead quote those, but we won't.
 * So we represent 140a by 140 and 140b by 141.
 */
course(140,  programming_languages,   [olsson, pandey]).
course(141,  programming_languages,   [olsson]).
course(142,  compiler_construction,   [pandey]).
course(240,  programming_languages,   [levitt]).
course(150,  operating_systems,       [levitt]).
course(122,  algorithms,              [gusfield,rogaway]).
course( 60,  data_structures,         [rogaway,davis]).
course( 20,  discrete_structures,     [gusfield,levitt,davis]).


test_courses :-
	test_courses1,
	test_courses2.

test_courses1 :-
	nl, printstring("TESTING C_s"), nl, nl,
	all1o(  c_numbers, N, "Number"),
	all1o(  c_pl, N, "Number"),
	all1o(  c_notpl, N, "Number"),
	all1o(  c_inst60, L, "List_of_60_Instructors"),
	all1o(  c_inst60_sorted, L, "Sorted_List_of_60_Instructors"),
	all1o(  c_inst20, L, "List_of_20_Instructors"),
	all1o(  c_inst20_sorted, L, "Sorted_List_of_20_Instructors"),
	all2io( c_inst_sorted, 60, L, "Sorted_List_of_Instructors"),
	all2io( c_inst_sorted,  20, L, "Sorted_List_of_Instructors"),
	all2io( c_inst_sorted, 140, L, "Sorted_List_of_Instructors").

test_courses2 :-
	all1o(  c_single_inst, N, "Number"),
	all1o(  c_multi_inst, N, "Number"),
	all2io( c_exclusive, levitt, N, "Number"),
	all2io( c_exclusive, pandey, N, "Number"),
	all2io( c_exclusive, rogaway, N, "Number"),
	all2io( c_exclusive, olsson, N, "Number"),
	all1o(  c_12_inst_1or, N, "Number"),
	all1o(  c_12_inst_2wo, N, "Number").

test_lists :-
	test_delete,
	test_delete_question,
        test_sortappend.

test_delete :-
	nl, printstring("TESTING DELETE"), nl, nl,
	all3iio( delete, [1], 1, Z, "Z"),
	all3iio( delete, [1], 2, Z, "Z"),
	all3iio( delete, [1,2,3], 2, Z, "Z"),
	all3iio( delete, [1,2,3], 3, Z, "Z"),
	all3iio( delete, [2, [1]], [1], Z, "Z"),
	all3iio( delete, [3, 4, 5], [1, 2], Z, "Z"),
	all3iio( delete, [3, [1,2], 5], [1, 2], Z, "Z").

test_delete_question :-
	nl, printstring("TESTING DELETE QUESTION"), nl, nl,
	all1ostring( delete_question, _Answer, "Answer").

% recall that builing sort drops duplicates
% and puts longer lists before shorter lists.
test_sortappend :-
	nl, printstring("TESTING SORTAPPEND"), nl, nl,
	all3iio( sortappend, [], [], Z, "Z"),
	all3iio( sortappend, [1], [2], Z, "Z"),
	all3iio( sortappend, [2], [1,3], Z, "Z"),
	all3iio( sortappend, [2], [1,2,3], Z, "Z"),
	all3iio( sortappend, [1,3,5], [2,4,6], Z, "Z"),
	all3iio( sortappend, [1,[4],3,[],[4]], [2,[4]], Z, "Z"),
	all3iio( sortappend, [1,3,[5]], [2,[4]], Z, "Z"),
	all3iio( sortappend, [1,[],[4,1],[5]], [2,[4]], Z, "Z").


test_distribute :-
	nl, printstring("TESTING DISTRIBUTE"), nl, nl,
	all3iio( distribute, aaaa, [], Z, "Z"),
	all3iio( distribute, aaaa, [b], Z, "Z"),
	all3iio( distribute, aaaa, [b,c], Z, "Z"),
	all3iio( distribute, 4, [1,2,3], Z, "Z"),
	all3iio( distribute, 99, [11,22,33,44], Z, "Z").

test_myfor :-
	nl, printstring("TESTING MYFOR"), nl, nl,
	all3iio( myfor, 2,10,Z, "Z"),
	all3iio( myfor, 3,7,Z, "Z"),
	all3iio( myfor, 3,2,Z, "Z").

test_crossmyfor :-
	nl, printstring("TESTING CROSSMYFOR"), nl, nl,
	all3iio( crossmyfor, 0, 0, Z, "Z"),
	all3iio( crossmyfor, 1, 0, Z, "Z"),
	all3iio( crossmyfor, 0, 2, Z, "Z"),
	all3iio( crossmyfor, 1, 2, Z, "Z"),
	all3iio( crossmyfor, 1, 2, Z, "Z"),
	all3iio( crossmyfor, 2, 1, Z, "Z"),
	all3iio( crossmyfor, 2, 2, Z, "Z"),
	all3iio( crossmyfor, 3, 2, Z, "Z"),
	all3iio( crossmyfor, 1, 3, Z, "Z"),
	all3iio( crossmyfor, 3, 1, Z, "Z"),
	all3iio( crossmyfor, 3, 3, Z, "Z").

test_getallmeetings :-
	nl, printstring("TESTING GETALLMEETINGS"), nl, nl,
        Jimi    = [jimi, [research]],
        Eva     = [eva, [admin,finance]],
        Nancy   = [nancy, [research,sales]],
        Ludwig  = [ludwig, [sales, marketing, admin]],
	C = [Jimi, Eva, Nancy],
	all2io( getallmeetings, [], Z, "Z"),
	all2io( getallmeetings, C, Z, "Z"),
	D = [Jimi, Eva, Nancy, Ludwig],
	all2io( getallmeetings, [], Z, "Z"),
	all2io( getallmeetings, D, Z, "Z").
	
test_participants :-
	nl, printstring("TESTING PARTICIPANTS"), nl, nl,
        Jimi    = [jimi, [research]],
        Eva     = [eva, [admin,finance]],
        Nancy   = [nancy, [research,sales]],
        Ludwig  = [ludwig, [sales, marketing, admin]],
	C = [Jimi, Eva, Nancy, Ludwig],
	all2io( participants, [], Z, "Z"),
	all2io( participants, C, Z, "Z").
	

test_osched :-
	test_sched_short(osched),
	test_sched_medium(osched),
	test_sched_long(osched).

test_xsched :-
	test_sched_short(xsched),
	test_sched_medium(xsched),
	test_sched_long(xsched).

/* some aliases */
tos :-
	test_sched_short(osched).
tom :-
	test_sched_medium(osched).
tol :-
	test_sched_long(osched).
txs :-
	test_sched_short(xsched).
txm :-
	test_sched_medium(xsched).
txl :-
	test_sched_long(xsched).


test_sched_short(Which) :-
	nl, printstring("TESTING SCHED (short) "), write(Which), nl, nl,
        Jimi    = [jimi, [research]],
        Eva     = [eva, [admin,finance]],

	B = [Jimi, Eva],

	printstring("  UNSORTED"), nl,
	all4iiio( Which, 1, 1, [], Z, "Z"),
	all4iiio( Which, 1, 1, B, Z, "Z"),
	all4iiio( Which, 1, 2, B, Z, "Z"),
	all4iiio( Which, 1, 3, B, Z, "Z"),
	all4iiio( Which, 1, 4, B, Z, "Z"),
	all4iiio( Which, 1, 5, B, Z, "Z"),

	printstring("  SORTED"), nl,
	sort4iiio( Which, 1, 1, [], Z, "Z"),
	sort4iiio( Which, 1, 1, B, Z, "Z"),
	sort4iiio( Which, 1, 2, B, Z, "Z"),
	sort4iiio( Which, 1, 3, B, Z, "Z"),
	sort4iiio( Which, 1, 4, B, Z, "Z"),
	sort4iiio( Which, 1, 5, B, Z, "Z").
	
test_sched_medium(Which) :-
	nl, printstring("TESTING SCHED (medium) "), write(Which), nl, nl,
        Jimi    = [jimi, [research]],
        Eva     = [eva, [admin,finance]],
        Nancy   = [nancy, [research,sales]],

	B = [Jimi, Eva],
	C = [Jimi, Eva, Nancy],

	printstring("  UNSORTED"), nl,
	all4iiio( Which, 2, 1, [], Z, "Z"),
	all4iiio( Which, 2, 1, B, Z, "Z"),
	all4iiio( Which, 2, 2, B, Z, "Z"),
	all4iiio( Which, 3, 1, B, Z, "Z"),
	all4iiio( Which, 3, 2, B, Z, "Z"),

	all4iiio( Which, 1, 2, C, Z, "Z"),
	all4iiio( Which, 1, 3, C, Z, "Z"),
	all4iiio( Which, 2, 1, C, Z, "Z"),
	all4iiio( Which, 2, 2, C, Z, "Z"),

	printstring("  SORTED"), nl,
	sort4iiio( Which, 2, 1, [], Z, "Z"),
	sort4iiio( Which, 2, 1, B, Z, "Z"),
	sort4iiio( Which, 2, 2, B, Z, "Z"),
	sort4iiio( Which, 3, 1, B, Z, "Z"),
	sort4iiio( Which, 3, 2, B, Z, "Z"),

	sort4iiio( Which, 1, 2, C, Z, "Z"),
	sort4iiio( Which, 1, 3, C, Z, "Z"),
	sort4iiio( Which, 2, 1, C, Z, "Z"),
	sort4iiio( Which, 2, 2, C, Z, "Z").
	
test_sched_long(Which) :-
	nl, printstring("TESTING SCHED (long) "), write(Which), nl, nl,
        Jimi    = [jimi, [research]],
        Eva   = [eva, [admin,finance]],
        Nancy   = [nancy, [research,sales]],
        Ludwig  = [ludwig, [sales, marketing, admin]],
	C = [Jimi, Eva, Nancy, Ludwig],

	printstring("  UNSORTED"), nl,
	all4iiio( Which, 1, 1, [], Z, "Z"),
	all4iiio( Which, 1, 1, C, Z, "Z"),
	all4iiio( Which, 1, 2, C, Z, "Z"),
	all4iiio( Which, 1, 3, C, Z, "Z"),
	all4iiio( Which, 1, 4, C, Z, "Z"),
	all4iiio( Which, 1, 5, C, Z, "Z"),

     	printstring("  SORTED"), nl,
	sort4iiio( Which, 1, 1, [], Z, "Z"),
	sort4iiio( Which, 1, 1, C, Z, "Z"),
	sort4iiio( Which, 1, 2, C, Z, "Z"),
	sort4iiio( Which, 1, 3, C, Z, "Z"),
	sort4iiio( Which, 1, 4, C, Z, "Z"),
	sort4iiio( Which, 1, 5, C, Z, "Z").

/* semi-general tester.
 * run Predicate on A1, A2, A3, A4
 * assume output goes only to A3 and A4,
 * whose names are in the string SA3 and SA4.
 */
all4iioo(Predicate,A1,A2,A3,A4,SA3,SA4) :-
	printstring("testing: "), write(Predicate),
	printstring("("), write(A1),
	printstring(","), write(A2),
	printstring(","), printstring(SA3),
	printstring(","), printstring(SA4),
	printstring(")"), nl,
	T =..[Predicate,A1,A2,A3,A4], call(T),
	printstring(SA3), printstring(" = "),
	write(A3), nl, nl,
	printstring(SA4), printstring(" = "),
	write(A4), nl, nl,
	fail; true.

/* semi-general tester.
 * run Predicate on A1, A2, A3, A4
 * assume output goes only to A4, whose name is in the string SA4.
 * like all4iiio, but sorts.
 */
sort4iiio(Predicate,A1,A2,A3,A4,SA4) :-
	printstring("testing: "), write(Predicate),
	printstring("("), write(A1),
	printstring(","), write(A2),
	printstring(","), write(A3),
	printstring(","), printstring(SA4),
	printstring(")"), nl,
	T =..[Predicate,A1,A2,A3,A4],
	findall(A4,call(T),L),
	sort(L,SL),
	printstring("SL"), printstring(" = "),
	write(SL), nl, nl,
	fail; true.

/* semi-general tester.
 * run Predicate on A1, A2, A3, A4
 * assume output goes only to A4, whose name is in the string SA4.
 */
all4iiio(Predicate,A1,A2,A3,A4,SA4) :-
	printstring("testing: "), write(Predicate),
	printstring("("), write(A1),
	printstring(","), write(A2),
	printstring(","), write(A3),
	printstring(","), printstring(SA4),
	printstring(")"), nl,
	T =..[Predicate,A1,A2,A3,A4], call(T),
	printstring(SA4), printstring(" = "),
	write(A4), nl, nl,
	fail; true.

/* semi-general tester.
 * run Predicate on A1, A2, A3
 * assume output goes only to A3, whose name is in the string SA3.
 */
all3iio(Predicate,A1,A2,A3,SA3) :-
	printstring("testing: "), write(Predicate),
	printstring("("), write(A1),
	printstring(","), write(A2),
	printstring(","), printstring(SA3),
	printstring(")"), nl,
	T =..[Predicate,A1,A2,A3], call(T),
	printstring(SA3), printstring(" = "),
	write(A3), nl, nl,
	fail; true.

/* semi-general tester.
 * run Predicate on A1, A2, A3
 * assume output goes only to A1, whose name is in the string SA1.
 */
all3oii(Predicate,A1,A2,A3,SA1) :-
	printstring("testing: "), write(Predicate),
	printstring("("), printstring(SA1),
	printstring(","), write(A2),
	printstring(","), write(A3),
	printstring(")"), nl,
	T =..[Predicate,A1,A2,A3], call(T),
	printstring(SA1), printstring(" = "),
	write(A1), nl, nl,
	fail; true.

/* semi-general tester.
 * run Predicate on A1, A2, A3
 * assume output goes only to A2, whose name is in the string SA2.
 */
all3ioi(Predicate,A1,A2,A3,SA2) :-
	printstring("testing: "), write(Predicate),
	printstring("("), write(A1),
	printstring(","), printstring(SA2),
	printstring(","), write(A3),
	printstring(")"), nl,
	T =..[Predicate,A1,A2,A3], call(T),
	printstring(SA2), printstring(" = "),
	write(A2), nl, nl,
	fail; true.

/* semi-general tester.
 * run Predicate on A1, A2
 * assume output goes to A2, whose name is in the string SA2.
 */
all2io(Predicate,A1,A2,SA2) :-
	printstring("testing: "), write(Predicate),
	printstring("("), write(A1),
	printstring(","), printstring(SA2),
	printstring(")"), nl,
	T =..[Predicate,A1,A2], call(T),
	printstring(SA2), printstring(" = "),
	write(A2), nl, nl,
	fail; true.
	
/* semi-general tester.
 * run Predicate on A1, A2
 * assume output goes to A1, whose name is in the string SA1.
 */
all2oi(Predicate,A1,A2,SA1) :-
	printstring("testing: "), write(Predicate),
	printstring("("), printstring(SA1),
	printstring(","), write(A2),
	printstring(")"), nl,
	T =..[Predicate,A1,A2], call(T),
	printstring(SA1), printstring(" = "),
	write(A1), nl, nl,
	fail; true.
	
/* semi-general tester.
 * run Predicate on A1 and A2
 * assume yes or no as output.
 */
all2ii(Predicate,A1,A2) :-
	printstring("testing: "), write(Predicate),
	printstring("("), write(A1),
	printstring(","), write(A2),
	printstring(")"), nl,
	T =..[Predicate,A1,A2],
	((call(T),printstring("yes"));( \+ T ,printstring("no"))),
	nl, nl,
	fail; true.
	
/* semi-general tester.
 * run Predicate on A1
 * assume output goes only to A1, whose name is in the string SA1.
 */
all1o(Predicate,A1,SA1) :-
	printstring("testing: "), write(Predicate),
	printstring("("), printstring(SA1),
	printstring(")"), nl,
	T =..[Predicate,A1], call(T),
	printstring(SA1), printstring(" = "),
	write(A1), nl, nl,
	fail; true.
	
/* semi-general tester.
 * run Predicate on A1
 * assume output goes only to A1, whose name is in the string SA1.
 */
all1ostring(Predicate,A1,SA1) :-
	printstring("testing: "), write(Predicate),
	printstring("("), printstring(SA1),
	printstring(")"), nl,
	T =..[Predicate,A1], call(T),
	printstring(SA1), printstring(" = "),
	printstring(A1), nl, nl,
	fail; true.
	
/* a way to output strings. */
printstring([]).
printstring([H|T]) :- put(H), printstring(T).

% useful to save typing (but mainly for me, not students since they
% will rarely need to reload test.pl within a session) ...
yt :- consult('test.pl').
