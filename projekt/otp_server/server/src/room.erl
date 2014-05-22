-module(room).

-export([remove/3, removeFromAll/2, insert/5, receivers/3, findSock/2,
        findName/2, initSock/4, users_in_room/2, invite/3,
        add_socket/4, find/4]).

%%-ifdef(TEST).
%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").
%%-endif.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% returns a list of sockets in a room
% 
% ex. [{room_name, [sock1,sock2,sock3]}]
%
%--------------------------------------------------------------------------
receivers(Room, List, N) ->
    case lists:keyfind(Room, 1, List) of
        {Room, Tuple_List, _}->
            case N of
                1 ->
                    New_List = lists:map(fun({X, _}) -> X end, Tuple_List);
                2 ->
                    New_List = lists:map(fun({_, X}) -> X end, Tuple_List);
                _ ->
                    New_List = []
            end;
        false ->
            New_List = []
    end,
    New_List.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% initial insert into the List of rooms
% 
% ex. [{room_name, [sock1,sock2,sock3]}]
%
%--------------------------------------------------------------------------

initSock(Room, List, Socket, Name)->
    case findSock(Name, List) of
        false ->
            NewList = insert(Room, List, Socket, Name, false);
        _ ->
            NewName = string:concat(Name, "_"),
            NewList = initSock(Room, List, Socket, NewName)
    end,
    gen_server:cast(server, {'list_room_users', Room}),
    NewList.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% insert into the List of rooms
% 
% ex. [{room_name, [sock1,sock2,sock3]}]
%
%--------------------------------------------------------------------------
insert(Room, List, Socket, Name, Secrecy1) ->
    case lists:keyfind(Room, 1, List) of
        {Room, Sock_List, Secrecy2}->
            Tmp_List = lists:keydelete(Room, 1, List),
            New_List = [{Room, [{Socket, Name}|Sock_List], Secrecy2}|Tmp_List];
    
        false ->
            New_List = [{Room, [{Socket, Name}], Secrecy1}|List]
    end,
    New_List.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% remove the List of rooms
% 
% ex. [{room_name, [sock1,sock2,sock3]}]
%
%--------------------------------------------------------------------------
remove(Room, List, Socket)->
    case lists:keyfind(Room, 1, List) of
        {Room, Sock_List, Secrecy}->
            Tmp_List = lists:keydelete(Room, 1, List),
            Sock_List2 = lists:keydelete(Socket, 1, Sock_List),
            if 
                length(Sock_List2)==0 ->
                    New_List = Tmp_List;
                true ->  
                    New_List = [{Room, Sock_List2, Secrecy}|Tmp_List]
            end;
        false ->
            New_List = List
    end,
    New_List.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% remove the List of rooms
% 
% ex. [{room_name, [sock1,sock2,sock3]}]
%
%--------------------------------------------------------------------------
removeFromAll([], _) ->
    [];
removeFromAll([H|T], Sock) ->
    {Room, SockList, Secrecy} = H,
    case lists:keydelete(Sock, 1, SockList) of
        [] ->
            removeFromAll(T, Sock);
        NewSockList ->
            [{Room, NewSockList, Secrecy}|removeFromAll(T, Sock)]
    end.

%--------------------------------------------------------------------------
% Find the socket assosciated with the name
% Arg1 - username assosciated with the socket
% Arg2 - The entire list where the socket might be found
%--------------------------------------------------------------------------
findSock(Name, List) ->
    find(Name,List,2,1).

%--------------------------------------------------------------------------
% Find the name assosciated with the socket
% Arg1 - socket assosciated with the name
% Arg2 - The entire list where the socket might be found
%--------------------------------------------------------------------------
findName(Sock, List) ->
    find(Sock,List,1,2).

%--------------------------------------------------------------------------
% Find function used by findName/findSock
%--------------------------------------------------------------------------
find(Sock, List, Nr, Nr2) ->
    case lists:keyfind("global", 1, List) of
        {_, SockList, _} ->
            Tupel = lists:keyfind(Sock, Nr, SockList),
            element(Nr2,Tupel);
        false ->
            false
    end.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% Lists users in a room
%--------------------------------------------------------------------------

% Helper function to users_in_room
users_helper([H|[]], S) ->
    S ++ element(2,H);
users_helper([H|T], S) ->
    users_helper(T, S ++ element(2,H) ++ ",").	

users_in_room(Room ,List) ->
    {_, SockList, _} = lists:keyfind(Room, 1, List),
    "{"++ Room ++ " " ++ users_helper(SockList,"") ++ "}\n".
    
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% Find the invited socket, if it exists
%--------------------------------------------------------------------------
invite(Name, Room, List) ->
    case room:findSock(Name, List) of
        false ->
            NewList = List;
        Sock ->
            NewList = room:insert(Room, List, Sock, Name, true)
    end,
    gen_server:cast(server, {'list_room_users', Room}),
    NewList.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% Check things before we add a new socket
%--------------------------------------------------------------------------
add_socket(NewSock, Room, List, Secrecy1) ->
    case findName(NewSock, List) of
        false ->
            NewList = List;
        Name ->
            case lists:keyfind(Room, 1, List) of
                {_, SockList, Secrecy2} ->
                    case lists:keyfind(Name, 2, SockList) of
                        false ->
                            case Secrecy2 of
                                false ->
                                    gen_tcp:send(NewSock, "{success " ++ Room ++ "}\n"),
                                    NewList = insert(Room, List, NewSock, Name, Secrecy2);
                                true ->
                                    gen_tcp:send(NewSock, "{error " ++ Room ++ "}\n"),
                                    NewList = List
                            end;
                        _ ->
                            NewList = List
                    end;
                false ->
                    gen_tcp:send(NewSock, "{success " ++ Room ++ "}\n"),
                    NewList = insert(Room, List, NewSock, Name, Secrecy1)
            end
    end,
    gen_server:cast(server, {'list_room_users', Room}),
    NewList.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%-ifdef(TEST).
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

%%-endif.
