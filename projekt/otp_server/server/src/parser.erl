-module(parser).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

-export([handle/2, getString/3, getParts/1, sendBack/4, sendBack/5,
        sendBack/6]).

removeNewLine([]) ->
    [];
removeNewLine(Str) ->
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

getParts(BodyStr) ->
    Request = removeNewLine(string:sub_word(BodyStr, 1)),
    RoomName = removeNewLine(string:sub_word(BodyStr, 2)),
    case removeNewLine(string:sub_word(BodyStr, 3)) of
        "false" ->
            Secrecy = false;
        "true" ->
            Secrecy = true;
        Secrecy ->
            Secrecy
    end,
    {Request, RoomName, Secrecy}.

sendBack(Request, Data, Socket, Room) ->
    case Request of
        "/exitall" ->
            gen_server:cast(server, {'remove', Socket});
        _ ->
            gen_server:cast(server, {'send', Room, Data, Socket})   
    end.
sendBack(Request, Name, Data, Socket, Room) ->
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
sendBack(Request, RoomName, Secrecy, Data, Socket, Room) ->
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
    case getParts(BodyStr) of
        {[], [], []} ->
            gen_server:cast(server, {'send', Room, Data, Socket});
        {Request, [], []} ->
            sendBack(Request, Data, Socket, Room);
        {Request, Name, []} ->
            sendBack(Request, Name, Data, Socket, Room);
        {Request, Name, Secrecy} ->
            sendBack(Request, Name, Secrecy, Data, Socket, Room)
    end.

%% ------------------------------------------------------------------
%% Get the string from parser with the right name added to it
%%
%% ------------------------------------------------------------------
getString(FromParser, Sock, List) ->
    Room = string:sub_word(FromParser, 1),
    Len = string:len(Room),
    Msg = string:substr(FromParser, Len+1),
    Name = room:findName(Sock, List),
    NameMsg = string:join([Room, string:concat(Name, ">"), Msg], " "),
    NameMsg.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rooms_test_() ->
ok.


    
