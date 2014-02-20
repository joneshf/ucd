%%% ECS 140A HW6
%%% Hardy Jones

%% Part 1
%% Numbers of all courses.
c_numbers(N) :-
    course(N, _Name, _Profs).

%% Numbers of all programming language courses.
c_pl(N) :-
    course(N, programming_languages, _Profs).

%% Numbers of all non-programming language courses.
c_notpl(N) :-
    course(N, _Name, _Profs),
    \+(c_pl(N)).

%% List of those teaching ECS60.
c_inst60(L) :-
    course(60, _Name, L).

%% Sorted list of those teaching ECS60.
c_inst60_sorted(L) :-
    course(60, _Name, UnsortedL),
    sort(UnsortedL, L).

%% List of those teaching ECS20.
c_inst20(L) :-
    course(20, _Name, L).

%% Sorted list of those teaching ECS20.
c_inst20_sorted(L) :-
    course(20, _Name, UnsortedL),
    sort(UnsortedL, L).

%% Sorted list of those teaching `N`.
c_inst_sorted(N, L) :-
    course(N, _Name, UnsortedL),
    sort(UnsortedL, L).

%% Numbers of courses with exactly one instructor.
c_single_inst(N) :-
    course(N, _Name, Profs),
    length(Profs, 1).

%% Numbers of courses with more than one instructor.
c_multi_inst(N) :-
    course(N, _Name, Profs),
    length(Profs, Num),
    Num > 1.

%% Numbers of courses for which `I` is the only instructor.
c_exclusive(I, N) :-
    course(N, _Name, [I]).

%% Numbers of courses with exactly one or two instructors.
c_12_inst_1or(N) :-
    course(N, _Name, [_Prof]);
    course(N, _Name, [_Prof1, _Prof2]).

%% Numbers of courses with exactly one or two instructors.
c_12_inst_2wo(N) :-
    course(N, _Name, [_Prof]).
c_12_inst_2wo(N) :-
    course(N, _Name, [_Prof, _Prof2]).

%% Part 2

%% Which version of delete?
delete_question("It uses the version which doesn't fail if the element is not in the list.").

%% Append two lists and sort them.
sort_append(L1, L2, Ls) :-
    append(L1, L2, Appended),
    sort(Appended, Ls).

%% Part 3

%% Map over the second argument cons-ing on the first argument.
distribute(_W, [], []).
distribute(W, [H|T], [[W,H]|Rest]) :-
    distribute(W, T, Rest).

%% Part 4

%% Generates a range from `L` to `U` only in increasing order.
myfor(L, U, []) :-
    L > U.
myfor(L, U, [L | Res1]) :-
    L =< U,
    L1 is L + 1,
    myfor(L1, U, Res1).

%% Generates the cross product of [1..X] and [1..Y].
crossmyfor(X, Y, Z) :-
    myfor(1, X, Xs),
    myfor(1, Y, Ys),
    cross(Xs, Ys, Z).

cross([], _Ys, []).
cross(_Xs, [], []).
cross([X|Xs], Ys, Next) :-
    distribute(X, Ys, Comb),
    append(Comb, Combs, Next),
    cross(Xs, Ys, Combs).

%% Part 5

%% Part 5a

%% Returns the sorted list of unique meeting names.
getallmeetings(People, Meetings) :-
    get_meetings(People, Nested),
    flatten(Nested, Flattened),
    sort(Flattened, Meetings).

get_meetings([], []).
get_meetings([[_Person, Meetings]|Others], [Meetings|MoreMeetings]) :-
    get_meetings(Others, MoreMeetings).

%% Part 5b

%% Returns the list of meetings and their participants.
participants(People, Participants) :-
    % Grab every meeting.
    getallmeetings(People, Meetings),
    % Grab every person for each meeting.
    relate(Meetings, People, UnsortedParticipants),
    sort(UnsortedParticipants, Participants).

%% Returns every person for each meeting.
relate([], _People, []).
relate(_Meetings, [], []).
relate([Meeting|Meetings], People, [[Meeting, SortedParticipants]|Participants]) :-
    % Filter people that are in the meeting.
    filter_people(People, Meeting, SomeParticipants),
    sort(SomeParticipants, SortedParticipants),
    relate(Meetings, People, Participants).

filter_people([], _Meeting, []).
filter_people([[Person, Meetings]|People], Meeting, [Person|Participants]) :-
    member(Meeting, Meetings),
    filter_people(People, Meeting, Participants).
filter_people([_NoBueno|People], Meeting, Participants) :-
    filter_people(People, Meeting, Participants).
