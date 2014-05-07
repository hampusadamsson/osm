-module(room).

%% ------------------------------------------------------------------
%% Room Function Exports
%% ------------------------------------------------------------------
-export([addToRoom/4, findRoom/2, getRoomName/1, removeSocket/2]).

getRoomName(Msg) ->
    string:sub_word(Msg, 1).

addToRoom(RoomName1, Sock, [], AllRooms2) ->
    [{RoomName1, [Sock]}|AllRooms2];
addToRoom(RoomName1, Sock, AllRooms1, AllRooms2) ->
    [H|T] = AllRooms1,
    {RoomName2, SockList} = H,
    if
        RoomName1 =:= RoomName2 ->
            [{RoomName2, [Sock|SockList]}|T];
        true ->
            [H|addToRoom(RoomName1, Sock, T, AllRooms2)]
    end.

findRoom([], _) ->
    [];
findRoom(AllRooms, RoomName1) ->
    [H|T] = AllRooms,
    {RoomName2, SockList} = H,
    if
        RoomName1 =:= RoomName2 ->
            SockList;
        true ->
            findRoom(T, RoomName1)
    end.

removeSocket([], SockList, _) ->
    SockList;
removeSocket([H|T], SockList, Sock) ->
    if
        H =:= Sock ->
            T;
        true ->
            removeSocket(T, SockList, Sock)
    end.

removeSocket([H|T], Sock) ->
    {_, SockList} = H,
    case lists:member(Sock, SockList) of
        true ->
            removeSocket(SockList, SockList, Sock);
        false ->
            removeSocket(T, Sock)
    end.

