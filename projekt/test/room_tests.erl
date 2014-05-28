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
    ?assertEqual(room:receivers("", [], 1), []),
    ?assertEqual(room:receivers("", List1, 1), []),
    ?assertEqual(room:receivers("Room1", List2, 1), [s6]),
    ?assertEqual(room:receivers("Room1", List3, 1), [s6]),
    ?assertEqual(room:receivers("Room2", List3, 1), [s7]),
    ?assertEqual(room:receivers("global", List1, 1), Rec1),
    ?assertEqual(room:receivers("global", List2, 1), Rec1),
    ?assertEqual(room:receivers("global", List3, 1), Rec1),
    ?assertEqual(room:receivers("Room1", List2, 2), ["Kenny"]),
    ?assertEqual(room:receivers("Room1", List3, 2), ["Kenny"]),
    ?assertEqual(room:receivers("Room2", List3, 2), ["Timmy"]),
    ?assertEqual(room:receivers("global", List1, 2), Rec2),
    ?assertEqual(room:receivers("global", List2, 2), Rec2),
    ?assertEqual(room:receivers("global", List3, 2), Rec2).

insert_test() ->
    A1 = room:insert("global", [], s1, "Tommy", false),
    A2 = [
        {"global",[{s1, "Tommy"}], false}
    ],
    B1 = room:insert("Room1", A1, s2, "Timmy", true),
    B2 = [
        {"Room1",[{s2, "Timmy"}], true},
        {"global",[{s1, "Tommy"}], false}
    ],
    C1 = room:insert("global", B1, s3, "Kenny", false),
    C2 = [
        {"Room1",[{s2, "Timmy"}], true},
        {"global",[
            {s3, "Kenny"},
            {s1, "Tommy"}
        ], false}
    ],
    ?assertEqual(A1, A2),
    ?assertEqual(B1, B2),
    ?assertEqual(C1, C2).

remove_test() ->
    L1 = room:insert("global", [], s1, "Tommy", false),
    L2 = room:insert("global", L1, s2, "Timmy", false),
    L3 = room:insert("global", L2, s3, "Kenny", false),
    L4 = room:insert("global", L3, s4, "Jenny", false),
    L5 = room:insert("global", L4, s5, "Ronny", false),
    L6 = room:remove("global", L5, s2),
    L7 = room:remove("global", L6, s4),
    L8 = room:remove("global", L7, s5),
    L9 = room:remove("global", L8, s1),
    Not1 = room:remove("globak", L9, s3),
    Not2 = room:remove("global", L9, s6),
    ?assertEqual(Not1, [{"global", [{s3, "Kenny"}], false}]),
    ?assertEqual(Not2, [{"global", [{s3, "Kenny"}], false}]),
    ?assertEqual(L9, [{"global", [{s3, "Kenny"}], false}]).

remove_from_all_test() ->
    L1 = room:insert("global", [], s1, "Tommy", false),
    L2 = room:insert("Room1", L1, s1, "Tommy", false),
    L3 = room:insert("Room2", L2, s1, "Tommy", false),
    L4 = room:insert("Room3", L3, s1, "Tommy", false),
    L5 = room:insert("Room4", L4, s1, "Tommy", false),
    ?assertEqual(room:remove_from_all([], s2), []),
    ?assertEqual(room:remove_from_all(L5, s2), L5),
    ?assertEqual(room:remove_from_all(L1, s1), []),
    ?assertEqual(room:remove_from_all(L5, s1), []).

find_sock_test() ->
    L1 = room:insert("global", [], s1, "Tommy", false),
    L2 = room:insert("global", L1, s2, "Timmy", false),
    L3 = room:insert("global", L2, s3, "Kenny", false),
    L4 = room:insert("global", L3, s4, "Jenny", false),
    L5 = room:insert("global", L4, s5, "Ronny", false),
    ?assertEqual(room:find_sock("", []), false),
    ?assertEqual(room:find_sock("Benny", L5), false),
    ?assertEqual(room:find_sock("Tommy", L5), s1),
    ?assertEqual(room:find_sock("Timmy", L5), s2),
    ?assertEqual(room:find_sock("Kenny", L5), s3),
    ?assertEqual(room:find_sock("Jenny", L5), s4),
    ?assertEqual(room:find_sock("Ronny", L5), s5).

find_name_test() ->
    L1 = room:insert("global", [], s1, "Tommy", false),
    L2 = room:insert("global", L1, s2, "Timmy", false),
    L3 = room:insert("global", L2, s3, "Kenny", false),
    L4 = room:insert("global", L3, s4, "Jenny", false),
    L5 = room:insert("global", L4, s5, "Ronny", false),
    ?assertEqual(room:find_name(s1, []), false),
    ?assertEqual(room:find_name(s6, L5), false),
    ?assertEqual(room:find_name(s1, L5), "Tommy"),
    ?assertEqual(room:find_name(s2, L5), "Timmy"),
    ?assertEqual(room:find_name(s3, L5), "Kenny"),
    ?assertEqual(room:find_name(s4, L5), "Jenny"),
    ?assertEqual(room:find_name(s5, L5), "Ronny").

users_in_room_test() ->
    L1 = room:insert("global", [], s1, "Tommy", false),
    L2 = room:insert("global", L1, s2, "Timmy", false),
    L3 = room:insert("global", L2, s3, "Kenny", false),
    L4 = room:insert("global", L3, s4, "Jenny", false),
    L5 = room:insert("global", L4, s5, "Ronny", false),
    CorrectStr = "{global Ronny,Jenny,Kenny,Timmy,Tommy}\n",
    ?assertEqual(room:users_in_room("global", []), false),
    ?assertEqual(room:users_in_room("globak", L5), false),
    ?assertEqual(room:users_in_room("global", L5), CorrectStr).

rooms_test() ->
    L1 = room:insert("global", [], s1, "Tommy", false),
    L2 = room:insert("global", L1, s2, "Timmy", false),
    L3 = room:insert("global", L2, s3, "Kenny", false),
    L4 = room:insert("global", L3, s4, "Jenny", false),
    L5 = room:insert("Room1", L4, s1, "Tommy", false),
    L6 = room:insert("Room2", L5, s2, "Timmy", false),
    L7 = room:insert("Room3", L6, s3, "Kenny", false),
    L8 = room:insert("Room4", L7, s4, "Jenny", false),
    CorrectStr1 = "{Room4,Room3,Room2,Room1,global}\n",
    CorrectStr2 = "{track Room3,global}\n",
    ?assertEqual(room:rooms([], false), false),
    ?assertEqual(room:rooms([], "Kenny"), false),
    ?assertEqual(room:rooms(L8, false), CorrectStr1),
    ?assertEqual(room:rooms(L8, "Kenny"), CorrectStr2).

get_info_test() ->
    L1 = room:insert("global", [], s1, "Tommy", false),
    L2 = room:insert("global", L1, s2, "Timmy", false),
    L3 = room:insert("global", L2, s3, "Kenny", false),
    L4 = room:insert("global", L3, s4, "Jenny", false),
    L5 = room:insert("Room1", L4, s1, "Tommy", true),
    L6 = room:insert("Room2", L5, s2, "Timmy", false),
    L7 = room:insert("Room3", L6, s3, "Kenny", true),
    L8 = room:insert("Room4", L7, s4, "Jenny", false),
    CorrectStr1 = "Room1 >  INFO Room1: private\n",
    CorrectStr2 = "Room2 >  INFO Room2: public\n",
    CorrectStr3 = "Room3 >  INFO Room3: private\n",
    CorrectStr4 = "Room4 >  INFO Room4: public\n",
    ?assertEqual(room:get_info("Room1", L8), CorrectStr1),
    ?assertEqual(room:get_info("Room2", L8), CorrectStr2),
    ?assertEqual(room:get_info("Room3", L8), CorrectStr3),
    ?assertEqual(room:get_info("Room4", L8), CorrectStr4).

