-module(room_tests).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
receivers_test() ->
    List1 = [
        {"global",[
            {s1, "Maria"},
            {s2, "Erik"},
            {s3, "Hampus"},
            {s4, "John"},
            {s5, "Per Albin"}
        ], false}
    ],
    List2 = [
        {"global",[
            {s1, "Maria"},
            {s2, "Erik"},
            {s3, "Hampus"},
            {s4, "John"},
            {s5, "Per Albin"}
        ], false},
        {"Room1",[{s6, "Kenny"}], true}
    ],
    List3 = [
        {"Room2",[{s7, "Timmy"}], false},
        {"global",[
            {s1, "Maria"},
            {s2, "Erik"},
            {s3, "Hampus"},
            {s4, "John"},
            {s5, "Per Albin"}
        ], false},
        {"Room1",[{s6, "Kenny"}], true}
    ],
    Rec1 = [s1, s2, s3, s4, s5],
    Rec2 = ["Maria", "Erik", "Hampus", "John", "Per Albin"],
    ?assertEqual(receivers("", [], 1), []),
    ?assertEqual(receivers("", List1, 1), []),
    ?assertEqual(receivers("Room1", List2, 1), [s6]),
    ?assertEqual(receivers("Room1", List3, 1), [s6]),
    ?assertEqual(receivers("Room2", List3, 1), [s7]),
    ?assertEqual(receivers("global", List1, 1), Rec1),
    ?assertEqual(receivers("global", List2, 1), Rec1),
    ?assertEqual(receivers("global", List3, 1), Rec1),
    ?assertEqual(receivers("Room1", List2, 2), ["Kenny"]),
    ?assertEqual(receivers("Room1", List3, 2), ["Kenny"]),
    ?assertEqual(receivers("Room2", List3, 2), ["Timmy"]),
    ?assertEqual(receivers("global", List1, 2), Rec2),
    ?assertEqual(receivers("global", List2, 2), Rec2),
    ?assertEqual(receivers("global", List3, 2), Rec2).

insert_test() ->
    First = [],
    A1 = insert("global", First, s1, "Tommy", false),
    A2 = [
        {"global",[{s1, "Tommy"}], false}
    ],
    B1 = insert("Room1", A1, s2, "Timmy", true),
    B2 = [
        {"Room1",[{s2, "Timmy"}], true},
        {"global",[{s1, "Tommy"}], false}
    ],
    C1 = insert("global", B1, s3, "Kenny", false),
    C2 = [
        {"global",[
            {s3, "Kenny"},
            {s1, "Tommy"}
        ], false},
        {"Room1",[{s2, "Timmy"}], true}
    ],
    ?assertEqual(A1, A2),
    ?assertEqual(B1, B2),
    ?assertEqual(C1, C2).

remove_test() ->
    L1 = insert("global", [], s1, "Tommy", false),
    L2 = insert("global", L1, s2, "Timmy", false),
    L3 = insert("global", L2, s3, "Kenny", false),
    L4 = insert("global", L3, s4, "Jenny", false),
    L5 = insert("global", L4, s5, "Ronny", false),
    L6 = remove("global", L5, s2),
    L7 = remove("global", L6, s4),
    L8 = remove("global", L7, s5),
    L9 = remove("global", L8, s1),
    ?assertEqual(L9, [{"global", [{s3, "Kenny"}], false}]).

removeFromAll_test() ->
    L1 = insert("global", [], s1, "Tommy", false),
    L2 = insert("Room1", L1, s1, "Tommy", false),
    L3 = insert("Room2", L2, s1, "Tommy", false),
    L4 = insert("Room3", L3, s1, "Tommy", false),
    L5 = insert("Room4", L4, s1, "Tommy", false),
    ?assertEqual(removeFromAll(L1, s1), []),
    ?assertEqual(removeFromAll(L5, s1), []).

findSock_test() ->
    L1 = insert("global", [], s1, "Tommy", false),
    L2 = insert("global", L1, s2, "Timmy", false),
    L3 = insert("global", L2, s3, "Kenny", false),
    L4 = insert("global", L3, s4, "Jenny", false),
    L5 = insert("global", L4, s5, "Ronny", false),
    ?assertEqual(findSock("Benny", L5), false),
    ?assertEqual(findSock("Tommy", L5), s1),
    ?assertEqual(findSock("Timmy", L5), s2),
    ?assertEqual(findSock("Kenny", L5), s3),
    ?assertEqual(findSock("Jenny", L5), s4),
    ?assertEqual(findSock("Ronny", L5), s5).

findName_test() ->
    L1 = insert("global", [], s1, "Tommy", false),
    L2 = insert("global", L1, s2, "Timmy", false),
    L3 = insert("global", L2, s3, "Kenny", false),
    L4 = insert("global", L3, s4, "Jenny", false),
    L5 = insert("global", L4, s5, "Ronny", false),
    ?assertEqual(findName(s1, L5), "Tommy"),
    ?assertEqual(findName(s2, L5), "Timmy"),
    ?assertEqual(findName(s3, L5), "Kenny"),
    ?assertEqual(findName(s4, L5), "Jenny"),
    ?assertEqual(findName(s5, L5), "Ronny").

users_in_room_test() ->
    L1 = insert("global", [], s1, "Tommy", false),
    L2 = insert("global", L1, s2, "Timmy", false),
    L3 = insert("global", L2, s3, "Kenny", false),
    L4 = insert("global", L3, s4, "Jenny", false),
    L5 = insert("global", L4, s5, "Ronny", false),
    CorrectStr = "{global Ronny,Jenny,Kenny,Timmy,Tommy}\n",
    ?assertEqual(users_in_room("global", L5), CorrectStr).

