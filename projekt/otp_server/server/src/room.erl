-module(room).

-export([remove/3, remove_from_all/2, insert/5, receivers/3, find_sock/2,
        find_name/2, init_sock/4, users_in_room/2, invite/3,
        add_socket/4, find/4]).

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

init_sock(Room, List, Socket, Name)->
    case find_sock(Name, List) of
        false ->
            NewList = insert(Room, List, Socket, Name, false);
        _ ->
            NewName = string:concat(Name, "_"),
            NewList = init_sock(Room, List, Socket, NewName)
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
        {Room, Sock_List, Secrecy2} ->
            Tmp_List = lists:keydelete(Room, 1, List),
            case lists:keyfind(Socket, 1, Sock_List) of
                false ->
                    New_List = [{Room, [{Socket, Name}|Sock_List], Secrecy2}|Tmp_List];
                _ ->
                    New_List = List
            end;
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
    gen_server:cast(server, {'list_room_users', Room}),
    New_List.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% Update all rooms user lists
%
%--------------------------------------------------------------------------
inform_all([]) ->
    ok;
inform_all([{Room, _, _}|T]) ->
    gen_server:cast(server, {'list_room_users', Room}),
    inform_all(T).

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
    inform_all(List),
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
find(Sock, List, Nr, Nr2) ->
    case lists:keyfind("global", 1, List) of
        {_, SockList, _} ->
            case lists:keyfind(Sock, Nr, SockList) of
                false ->
                    false;
                Tupel ->
                    element(Nr2,Tupel)
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
    gen_server:cast(server, {'list_room_users', Room}),
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
    gen_server:cast(server, {'list_room_users', Room}),
    NewList.

