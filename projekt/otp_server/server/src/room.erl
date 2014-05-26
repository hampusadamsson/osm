-module(room).

-export([remove/3, remove_from_all/2, insert/5, receivers/3, find_sock/2,
        find_name/2, init_sock/4, users_in_room/2, invite/3,
        add_socket/4, rooms/1, get_info/2]).

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% returns a list of sockets in a room
% 
% ex. [{room_name, [sock1,sock2,sock3]}]
%
%--------------------------------------------------------------------------
receivers(Room, List, N) ->
    case lists:keyfind(Room, 1, List) of
        {Room, SockList, _}->
            case N of
                1 ->
                    NewList = lists:map(fun({X, _}) -> X end, SockList);
                2 ->
                    NewList = lists:map(fun({_, X}) -> X end, SockList);
                _ ->
                    NewList = []
            end;
        false ->
            NewList = []
    end,
    NewList.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% initial insert into the List of rooms
% 
% ex. [{room_name, [sock1,sock2,sock3]}]
%
%--------------------------------------------------------------------------

init_sock(Room, List, Sock, Name)->
    case find_sock(Name, List) of
        false ->
            NewList = insert(Room, List, Sock, Name, false);
        _ ->
            NewName = string:concat(Name, "_"),
            NewList = init_sock(Room, List, Sock, NewName)
    end,
    inform_all(NewList),
    NewList.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% insert into the List of rooms
% 
% ex. [{room_name, [sock1,sock2,sock3]}]
%
%--------------------------------------------------------------------------
insert(Room, List, Sock, Name, Secrecy1) ->
    case lists:keyfind(Room, 1, List) of
        {Room, SockList, Secrecy2} ->
            TmpList = lists:keydelete(Room, 1, List),
            case lists:keyfind(Sock, 1, SockList) of
                false ->
                    NewList = [{Room, [{Sock, Name}|SockList], Secrecy2}|TmpList];
                _ ->
                    NewList = List
            end;
        false ->
            NewList = [{Room, [{Sock, Name}], Secrecy1}|List]
    end,
    NewList.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% remove the List of rooms
% 
% ex. [{room_name, [sock1,sock2,sock3]}]
%
%--------------------------------------------------------------------------
remove(Room, List, Sock)->
    case lists:keyfind(Room, 1, List) of
        {Room, SockList1, Secrecy}->
            TmpList = lists:keydelete(Room, 1, List),
            SockList2 = lists:keydelete(Sock, 1, SockList1),
            case SockList2 of
                [] ->
                    NewList = TmpList,
                    inform_all(List);
                true ->  
                    NewList = [{Room, SockList2, Secrecy}|TmpList]
            end;
        false ->
            NewList = List
    end,
    inform_all(NewList),
    NewList.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% Update all rooms user lists
%
%--------------------------------------------------------------------------
inform_all_([]) ->
    ok;
inform_all_(NewList) ->
    [{Room, _, _}|T] = NewList,
    gen_server:cast(server, {'list_room_users', Room, NewList}),
    inform_all_(T).

inform_all(NewList) ->
    inform_all_(NewList),
    gen_server:cast(server, {'list_rooms', NewList}).

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% remove the List of rooms
% 
% ex. [{room_name, [sock1,sock2,sock3]}]
%
%--------------------------------------------------------------------------
remove_from_all_([], _) ->
    [];
remove_from_all_([H|T], Sock) ->
    {Room, SockList, Secrecy} = H,
    case lists:keydelete(Sock, 1, SockList) of
        [] ->
            remove_from_all_(T, Sock);
        NewSockList ->
            [{Room, NewSockList, Secrecy}|remove_from_all_(T, Sock)]
    end.

remove_from_all(List, Sock) ->
    NewList = remove_from_all_(List, Sock),
    inform_all(NewList),
    NewList.


%--------------------------------------------------------------------------
% Find the socket assosciated with the name
% Arg1 - username assosciated with the socket
% Arg2 - The entire list where the socket might be found
%--------------------------------------------------------------------------
find_sock(Name, List) ->
    find(Name,List,2,1).

%--------------------------------------------------------------------------
% Find the name assosciated with the socket
% Arg1 - socket assosciated with the name
% Arg2 - The entire list where the socket might be found
%--------------------------------------------------------------------------
find_name(Sock, List) ->
    find(Sock,List,1,2).

%--------------------------------------------------------------------------
% Find function used by find_name/find_sock
%--------------------------------------------------------------------------
find(Sock, List, N1, N2) ->
    case lists:keyfind("global", 1, List) of
        {_, SockList, _} ->
            case lists:keyfind(Sock, N1, SockList) of
                false ->
                    false;
                Tupel ->
                    element(N2,Tupel)
            end;
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

users_in_room(_, []) ->
    "";
users_in_room(Room ,List) ->
    case lists:keyfind(Room, 1, List) of
        false ->
            "";
        {_, SockList, _} ->
            "{"++ Room ++ " " ++ users_helper(SockList,"") ++ "}\n"
    end.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% Lists all rooms
%--------------------------------------------------------------------------

% Helper function to rooms
rooms_helper([]) ->
    "";
rooms_helper([{Room, _, _}|[]]) ->
    Room;
rooms_helper([{Room, _, _}|T]) ->
    Room ++ "," ++ rooms_helper(T).

rooms(List) ->
    "{" ++ rooms_helper(List) ++ "}\n".
    
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% Find the invited socket, if it exists
%--------------------------------------------------------------------------
invite(Name, Room, List) ->
    case room:find_sock(Name, List) of
        false ->
            NewList = List;
        Sock ->
            gen_tcp:send(Sock, "{invited " ++ Room ++ "}\n"),
            NewList = room:insert(Room, List, Sock, Name, false)
    end,
    inform_all(NewList),
    NewList.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% Check things before we add a new socket
%--------------------------------------------------------------------------
add_socket(NewSock, Room, List, Secrecy1) ->
    case find_name(NewSock, List) of
        false ->
            NewList = List;
        Name ->
            case lists:keyfind(Room, 1, List) of
                {_, SockList, Secrecy2} ->
                    case lists:keyfind(Name, 2, SockList) of
                        false ->
                            case Secrecy2 of
                                false ->
                                    gen_tcp:send(NewSock, "{success " ++ Room ++ " existant}\n"),
                                    NewList = insert(Room, List, NewSock, Name, Secrecy2);
                                true ->
                                    gen_tcp:send(NewSock, "{error " ++ Room ++ "}\n"),
                                    NewList = List
                            end;
                        _ ->
                            NewList = List
                    end;
                false ->
                    gen_tcp:send(NewSock, "{success " ++ Room ++ " new}\n"),
                    NewList = insert(Room, List, NewSock, Name, Secrecy1)
            end
    end,
    inform_all(NewList),
    NewList.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% Get info of a room as a string
%--------------------------------------------------------------------------
get_info(Room, List) ->
    {_, _, Secrecy} = lists:keyfind(Room, 1, List),
    case Secrecy of
        false ->
            Room ++ " >  INFO " ++ Room ++ ": open\n";
        true ->
            Room ++ " >  INFO " ++ Room ++ ": private\n"
    end.
