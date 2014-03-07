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
delete_question("It uses the version which doesn't fail if the element is not in the list. (4th Edition 145)").

delete1(_, [], []).
delete1(X, [X|L], M) :-
    !,
    delete1(X,L,M).
delete1(X, [Y|L1], [Y|L2]) :-
    delete1(X, L1, L2).

delete2(X, [X|Y], Y).
delete2(X, [Y|L1], [Y|L2]) :-
    delete2(X, L1, L2).

%% Append two lists and sort them.
sortappend(L1, L2, Ls) :-
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

in_meeting(Meeting, [_Person, Meetings]) :-
    member(Meeting, Meetings).

%% Returns every person for each meeting.
relate([], _People, []).
relate([Meeting|Meetings], People, [[Meeting,Persons]|Parts]) :-
    findall(Person,
            (member([Person|Ms], People), in_meeting(Meeting, [Person|Ms])),
            UnsortedPersons),
    sort(UnsortedPersons, Persons),
    relate(Meetings, People, Parts).

%% Part 5c

%% Schedules the meetings with overlap.
%% should return [[[Room, Hour], [Meeting, [Person]]]]
osched(_MR, _MH, [], []).
osched(MR, MH, Peeps, Schedule) :-
    participants(Peeps, Parts),
    crossmyfor(MR, MH, RoomHour),
    length(RoomHour, NumRH),
    length(Parts, NumParts),
    NumRH >= NumParts,
    join_up(RoomHour, Parts, Schedule),
    increasing_order(Schedule).
    %% all_different(Schedule).

join_up(_RoomHours, [], []).
join_up([], _Parts, []).
join_up(RoomHours, Parts, [[RH,Part]|Joined]) :-
    select(RH, RoomHours, RoomHours1),
    select(Part, Parts, Parts1),
    join_up(RoomHours1, Parts1, Joined).

increasing_order([]).
increasing_order([_]).
increasing_order([[[X0, Y0]|_Junk0],[[X1, Y1]|Rest]|Stuff]) :-
    X0 =< X1,
    Y0 =< Y1,
    increasing_order([[[X1, Y1]|Rest]|Stuff]).

increasing_order([[[X0, _Y0]|_Junk0],[[X1, Y1]|Rest]|Stuff]) :-
    X0 < X1,
    increasing_order([[[X1, Y1]|Rest]|Stuff]).

all_different([]).
all_different([H|T]) :-
    \+member(H, T),
    all_different(T).

%% Part 5d

%% Schedules the meetings with people exclusively in them.

xsched(MR, MH, Peeps, Overlapping) :-
    osched(MR, MH, Peeps, Overlapping),
    schedule_to_hours(Overlapping, Hrs0),
    msort(Hrs0, Hrs1),
    group(Hrs1, Hrs2),
    all_good(Hrs2).

schedule_to_hours([], []).
schedule_to_hours([[[_MR, MH],[_Meeting, People]]|More], [[MH, People]|Result]) :-
    schedule_to_hours(More, Result).

group([], []).
group([[N, Xs],[N, Ys]|More], Rest) :-
    append(Xs, Ys, Zs),
    group([[N, Zs]|More], Rest).
group([[N, Xs]|More], [[N, Xs]|Rest]) :-
    group(More, Rest).

all_good([]).
all_good([[_N, People]|More]) :-
    sort(People, People1),
    msort(People, People2),
    length(People1, Len1),
    length(People2, Len2),
    Len1 == Len2.
