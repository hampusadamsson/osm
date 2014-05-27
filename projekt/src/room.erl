-module(room).

-export([remove/3, remove_from_all/2, insert/5, receivers/3, find_sock/2,
	 find_name/2, init_sock/4, users_in_room/2, invite/3, add_socket/4, 
	 rooms/2, get_info/2, rename_user/3, get_ip/2]).

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
    inform_all(NewList),
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
            NewSockList = [{Sock, Name}|SockList],
            case lists:keyfind(Sock, 1, SockList) of
                false ->
                    NewList = lists:keyreplace(Room, 1, List, {Room, NewSockList, Secrecy2});
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
                SockList2 == [] ->
                    NewList = TmpList;
                true ->  
                    NewList = lists:keyreplace(Room, 1, List, {Room, SockList2, Secrecy})
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
    inform_all(T).

inform_all(NewList) ->
    inform_all_(NewList),
    gen_server:cast(server, {'list_rooms', NewList}).

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
    inform_all(NewList),
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
    inform_all(NewList),
    NewList.

%%--------------------------------------------------------------------------
%% @doc
%% Helper function to users_in_room
%% @end
%%--------------------------------------------------------------------------
users_helper([], []) ->
[];
users_helper([H|[]], S) ->
    S ++ element(2,H);
users_helper([H|T], S) ->
    users_helper(T, S ++ element(2,H) ++ ",").	

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
            "{"++ Room ++ " " ++ users_helper(SockList,"") ++ "}\n"
    end.
    
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% Lists all rooms, or if Name is not false then the Rooms user Name is in.
%--------------------------------------------------------------------------
room_string([]) ->
    "";
room_string([H|[]]) ->
    H;
room_string([H|T]) ->
    H ++ "," ++ room_string(T).

user_rooms([], _) ->
    [];
user_rooms([{Room, SockList, _}|T], Name) ->
    case lists:keymember(Name, 2, SockList) of
        false ->
            user_rooms(T, Name);
        true ->
            [Room|user_rooms(T, Name)]
    end.

rooms(List, Name) ->
    case Name of
        false ->
            RoomList = lists:map(fun({X, _, _}) -> X end, List),
            "{" ++ room_string(RoomList) ++ "}\n";
        _ ->
            RoomList = user_rooms(List, Name),
            "{track " ++ room_string(RoomList) ++ "}\n"
    end.

%%--------------------------------------------------------------------------
%% @doc
%% Returns a users ip and the port said user is connected through
%% @end
%%--------------------------------------------------------------------------
get_ip(Name, List) ->
    case room:find_sock(Name, List) of
        false ->
            false;
        Sock ->
            Info = inet:sockname(Sock),
            {_, {Ip, Port}} = Info,    
            Tmp = string:tokens(lists:flatten(io_lib:format("~p", [Ip])), ",{}"),
            Ip2 = string:join(Tmp,"."),
            {Ip2,Port}
    end.


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
            Room ++ " >  INFO " ++ Room ++ ": public\n";
        true ->
            Room ++ " >  INFO " ++ Room ++ ": private\n"
    end.

rename_user_(_, _, []) ->
    [];
rename_user_(New, Sock, List) ->
    [H|T] = List,
    {Room, SockList, Secrecy} = H,   
    case lists:keyfind(Sock, 1, SockList) of
        false ->
            [H|rename_user_(New, Sock, T)];
        {_, Current} ->
            [{
                Room,
                lists:keyreplace(Current, 2, SockList, {Sock, New}),
                Secrecy
            }|rename_user_(New, Sock, T)] 
    end.

rename_user(New1, Sock, List) ->
    case find_sock(New1, List) of
        false ->
            NewList = rename_user_(New1, Sock, List);
        _ ->
            New2 = string:concat(New1, "_"),
            NewList = rename_user_(New2, Sock, List)
    end,
    inform_all(NewList),
    NewList.

