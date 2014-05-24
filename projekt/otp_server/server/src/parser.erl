-module(parser).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

-export([handle/2, get_string/3]).

remove_new_line([]) ->
    [];
remove_new_line(Str) ->
    Len = string:len(Str),
    case string:substr(Str, Len) of
        "\n" ->
            if
                Len>1 ->
                    [Clean] = string:tokens(Str, "\n");
                true ->
                    Clean = string:tokens(Str, "\n")
            end;
        _ ->
            Clean = Str
    end,
    Clean.

get_parts(BodyStr) ->
    Request = remove_new_line(string:sub_word(BodyStr, 1)),
    RoomName = remove_new_line(string:sub_word(BodyStr, 2)),
    case remove_new_line(string:sub_word(BodyStr, 3)) of
        "true" ->
            Secrecy = true;
        [] ->
            Secrecy = [];
        _ ->
            Secrecy = false
    end,
    {Request, RoomName, Secrecy}.

send_back(Request, Data, Socket, Room) ->
    case Request of
        "/exitall" ->
            ok; %gen_server:cast(server, {'remove', Socket});
        _ ->
            gen_server:cast(server, {'send', Room, Data, Socket})   
    end.
send_back(Request, Name, Data, Socket, Room) ->
    case Request of
        "/join" ->
            gen_server:cast(server, {'add_socket', Name, Socket, false});
        "/invite" ->
            gen_server:cast(server, {'invite', Name, Room});
        "/exit" ->
            gen_server:cast(server, {'remove_from_room', Name, Socket});
        _ ->
            gen_server:cast(server, {'send', Room, Data, Socket})   
    end.
send_back(Request, RoomName, Secrecy, Data, Socket, Room) ->
    case Request of
        "/join" ->
            gen_server:cast(server, {'add_socket', RoomName, Socket, Secrecy});
        _ ->
            gen_server:cast(server, {'send', Room, Data, Socket})   
    end.

%% ------------------------------------------------------------------
%% Incomming Data - Data being a string
%%
%% Data will be parsed to execute functions
%%
%% ------------------------------------------------------------------
handle(Data, Socket)->   
    [Room|Body] = string:tokens(Data, " "),

    BodyStr = string:join(Body, " "),
    case get_parts(BodyStr) of
        {[], [], []} ->
            gen_server:cast(server, {'send', Room, Data, Socket});
        {Request, [], []} ->
            send_back(Request, Data, Socket, Room);
        {Request, Name, []} ->
            send_back(Request, Name, Data, Socket, Room);
        {Request, Name, Secrecy} ->
            send_back(Request, Name, Secrecy, Data, Socket, Room)
    end.

%% ------------------------------------------------------------------
%% Get the string from parser with the right name added to it
%%
%% ------------------------------------------------------------------
get_string(FromParser, Sock, List) ->
    Room = string:sub_word(FromParser, 1),
    Len = string:len(Room),
    Msg = string:substr(FromParser, Len+1),
    Name = room:find_name(Sock, List),
    NameMsg = string:join([Room, string:concat(Name, ">"), Msg], " "),
    NameMsg.

