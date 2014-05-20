-module(room).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

-export([remove/3, removeFromAll/2, insert/5, receivers/3, findSock/2,
        findName/2, initSock/4, isSecret/2, users_in_room/2, invite/3,
        add_socket/4]).

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% returns a list of sockets in a room
% 
% ex. [{room_name, [sock1,sock2,sock3]}]
%
%--------------------------------------------------------------------------
receivers(Room, List, N) ->
    case lists:keyfind(Room, N, List) of
        {Room, Tuple_List, _}->
            New_List = lists:map(fun({X, _}) -> X end, Tuple_List);
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
            insert(Room, List, Socket, Name, false);
        _ ->
            NewName = string:concat(Name, "_"),
            insert(Room, List, Socket, NewName, false)
    end.

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

% ------------------------------------------------------------------
% Find name connected to Sock
% ------------------------------------------------------------------
findName(Sock, List) ->
    {_, SockList, _} = lists:keyfind("global", 1, List),
    {_, Name} = lists:keyfind(Sock, 1, SockList),
    Name.

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
%--------------------------------------------------------------------------
% find socket with name Name in List
%--------------------------------------------------------------------------
findSock(_, []) ->
    false;
findSock(Name, List) ->
    {_, SockList, _} = hd(List),
    case lists:keyfind(Name, 2, SockList) of
        false ->
            findSock(Name, tl(List));
        {Sock, _} ->
            Sock
    end.

isSecret(Room, List) ->
    {_, _, Secrecy} = lists:keyfind(Room, 1, List),
    Secrecy.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% Lists users in a room
%--------------------------------------------------------------------------

% Helper function to users_in_room
users_helper([H|[]], S) ->
    S ++ element(2,H);
users_helper([H|T], S) ->
    users_helper(T, S ++ element(2,H) ++ " ").	

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
            List;
        Sock ->
            room:insert(Room, List, Sock, Name, true)
    end.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% Check things before we add a new socket
%--------------------------------------------------------------------------
add_socket(NewSock, Room, List, Secrecy) ->
    Name = room:findName(NewSock, List),
    case lists:keyfind(Room, 1, List) of
        {_, SockList, _} ->
            case lists:keyfind(Name, 2, SockList) of
                false ->
                    room:insert(Room, List, NewSock, Name, Secrecy);
                _ ->
                    List
            end;
        false ->
            room:insert(Room, List, NewSock, Name, Secrecy)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rooms_test_() ->
    A = [],
    B = insert(rum1, A, s1, "Tommy", false),
    _=?_assertEqual(B,[{rum1,[s1]}]),
        
    C = insert(rum2, B, s2, "Timmy", false),
    __=?_assertEqual(C,[{rum2,[s2]},{rum1,[s1]}]),

    D = insert(rum1, C, s3, "Tommy", false),
    ___=?_assertEqual(D,[{rum1,[s3,s1]},{rum2,[s2]}]),

    E1 = remove(rum123, D, s1),
    ____=?_assertEqual(E1,[{rum1,[s3,s1]},{rum2,[s2]}]),
    
    ____1=?_assertEqual(receivers(rum1,E1,1),[s3,s1]), 
    ____2=?_assertEqual(receivers(rumqwe1,E1,1),[]), 

    E = remove(rum1, D, s1),
    _____=?_assertEqual(E,[{rum1,[s3]},{rum2,[s2]}]),

    F = remove(rum1, E, s3),
    ______=?_assertEqual(F,[{rum2,[s2]}]),

    G = remove(rum2, F, s2),
    _______=?_assertEqual(G,[]).
    
