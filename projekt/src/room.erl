-module(room).

-export([remove/3, remove_from_all/2, insert/5, receivers/3, find_sock/2,
        find_name/2, init_sock/4, users_in_room/2, invite/3,
        add_socket/4, find/4]).

%%--------------------------------------------------------------------------
%% @doc 
%% returns a list of sockets in a room
%% 
%% ex. [{room_name, [sock1,sock2,sock3]}]
%% @end
%%--------------------------------------------------------------------------
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

%%--------------------------------------------------------------------------
%% @doc 
%% initial insert into the List of rooms
%% 
%% ex. [{room_name, [sock1,sock2,sock3]}]
%% @end
%%--------------------------------------------------------------------------
init_sock(Room, List, Sock, Name)->
    case find_sock(Name, List) of
        false ->
            NewList = insert(Room, List, Sock, Name, false);
        _ ->
            NewName = string:concat(Name, "_"),
            NewList = init_sock(Room, List, Sock, NewName)
    end,
    gen_server:cast(server, {'list_room_users', Room}),
    NewList.

%%--------------------------------------------------------------------------
%% @doc
%% insert into the List of rooms
%% 
%% ex. [{room_name, [sock1,sock2,sock3]}]
%% @end
%%--------------------------------------------------------------------------
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

%%--------------------------------------------------------------------------
%% @doc
%% remove the List of rooms
%% 
%% ex. [{room_name, [sock1,sock2,sock3]}]
%% @end
%%--------------------------------------------------------------------------
remove(Room, List, Sock)->
    case lists:keyfind(Room, 1, List) of
        {Room, SockList1, Secrecy}->
            TmpList = lists:keydelete(Room, 1, List),
            SockList2 = lists:keydelete(Sock, 1, SockList1),
            if 
                length(SockList2)==0 ->
                    NewList = TmpList;
                true ->  
                    NewList = [{Room, SockList2, Secrecy}|TmpList]
            end;
        false ->
            NewList = List
    end,
    gen_server:cast(server, {'list_room_users', Room}),
    NewList.

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

%%--------------------------------------------------------------------------
%% @doc
%% Helper function for remove_from_all.
%% @end
%%--------------------------------------------------------------------------
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

%%--------------------------------------------------------------------------
%% @doc
%% remove the List of rooms
%% 
%% ex. [{room_name, [sock1,sock2,sock3]}]
%% @end
%%--------------------------------------------------------------------------
remove_from_all(List, Sock) ->
    NewList = remove_from_all_(List, Sock),
    inform_all(List),
    NewList.

%%--------------------------------------------------------------------------
%% @doc
%% Find the socket assosciated with the name
%%
%% Name - username assosciated with the socket
%%
%% List - The entire list where the socket might be found
%% @end
%%--------------------------------------------------------------------------
find_sock(Name, List) ->
    find(Name,List,2,1).

%%--------------------------------------------------------------------------
%% @doc
%% Find the name assosciated with the socket
%% 
%% Sock - socket assosciated with the name
%%
%% List - The entire list where the socket might be found
%% @end
%%--------------------------------------------------------------------------
find_name(Sock, List) ->
    find(Sock,List,1,2).

%%--------------------------------------------------------------------------
%% @doc
%% Find function used by findName/findSock
%% @end
%%--------------------------------------------------------------------------
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

%%--------------------------------------------------------------------------
%% @doc
%% Helper function to users_in_room
%% @end
%%--------------------------------------------------------------------------
users_in_room_([H|[]], S) ->
    S ++ element(2,H);
users_in_room_([H|T], S) ->
    users_in_room_(T, S ++ element(2,H) ++ ",").	

%%--------------------------------------------------------------------------
%% @doc
%% Lists users in a room
%% @end
%%--------------------------------------------------------------------------
users_in_room(_, []) ->
    "";
users_in_room(Room ,List) ->
    case lists:keyfind(Room, 1, List) of
        false ->
            "";
        {_, SockList, _} ->
            "{"++ Room ++ " " ++ users_in_room_(SockList,"") ++ "}\n"
    end.
    

%%--------------------------------------------------------------------------
%% @doc
%% Find the invited socket, if it exists
%% @end
%%--------------------------------------------------------------------------
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

%%--------------------------------------------------------------------------
%% @doc
%% Check things before we add a new socket
%% @end
%%--------------------------------------------------------------------------
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
