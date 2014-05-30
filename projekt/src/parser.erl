-module(parser).
%% ------------------------------------------------------------------
%% To use EUnit we must include this:
%% ------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

-export([remove_new_line/1, get_parts/1, handle/2, get_string/3]).


%% ------------------------------------------------------------------
%% @doc Removes the newline character at the end of the string Str
%%      and returns it.
%%
%%      Str - The string to be modified.
%% @end
%% ------------------------------------------------------------------
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

%% ------------------------------------------------------------------
%% @doc Extracts the first three words in a string and returns them
%%      as three seprate strings. The rest of the input string will
%%      be ignored.
%%
%%      BodyStr - The string of which the three words will be
%%      extracted from.
%% @end
%% ------------------------------------------------------------------
get_parts(BodyStr) ->
    Request = remove_new_line(string:sub_word(BodyStr, 1)),
    RoomName = remove_new_line(string:sub_word(BodyStr, 2)),
    case remove_new_line(string:sub_word(BodyStr, 3)) of
        "private" ->
            Secrecy = true;
        [] ->
            Secrecy = [];
        _ ->
            Secrecy = false
    end,
    {Request, RoomName, Secrecy}.

%% ------------------------------------------------------------------
%% @doc Matches out what command if any was given in the message then
%%      calls on the appropriate function to carry out the correct
%%      corresponding action.
%%
%%      Request - The actual command part of the message
%%
%%      Data - The remaining part of the message after the command
%%
%%      Socket - The socket form which the message came
%%
%%      Room - The rrom form which the message came
%% @end
%% ------------------------------------------------------------------
send_back(Request, Data, Socket, Room) ->
    case Request of
        "/info" ->
            gen_server:cast(server, {'info', Room, Socket});   
        _ ->
            gen_server:cast(server, {'send', Room, Data, Socket})   
    end.

%% ------------------------------------------------------------------
%% @doc Matches out what command if any was given in the message then
%%      calls on the appropriate function to carry out the correct
%%      corresponding action.
%%
%%      Request - The actual command part of the message
%%
%%      Name - The user which the command will effect
%%
%%      Data - The remaining part of the message after the command
%%
%%      Socket - The socket form which the message came
%%
%%      Room - The rrom form which the message came
%% @end
%% ------------------------------------------------------------------
send_back(Request, Name, Data, Socket, Room) ->
    case Request of
        "/join" ->
            gen_server:cast(server, {'add_socket', Name, Socket, false});
        "/invite" ->
            gen_server:cast(server, {'invite', Name, Room});
        "/rename" ->
            gen_server:cast(server, {'rename_user', Name, Socket});
        "/exit" ->
            gen_server:cast(server, {'remove_from_room', Name, Socket});
        "/whois" -> 
            gen_server:cast(server, {'whois', Name, Socket});
        "/track" -> 
            gen_server:cast(server, {'track', Name, Socket});
        _ ->
            gen_server:cast(server, {'send', Room, Data, Socket})   
    end.

%% ------------------------------------------------------------------
%% @doc Matches out what command if any was given in the message then
%%      calls on the appropriate function to carry out the correct
%%      corresponding action.
%%
%%      Request - The actual command part of the message
%%
%%      RoomName - The room that the command will effect
%%
%%      Secrecy - true or false depending on whether the room should
%%      be hidden or not
%%
%%      Data - The remaining part of the message after the command
%%
%%      Socket - The socket form which the message came
%%
%%      Room - The rrom form which the message came
%% @end
%% ------------------------------------------------------------------
send_back(Request, RoomName, Secrecy, Data, Socket, Room) ->
    case Request of
        "/join" ->
            gen_server:cast(server, {'add_socket', RoomName, Socket, Secrecy});
        _ ->
            gen_server:cast(server, {'send', Room, Data, Socket})   
    end.

%% ------------------------------------------------------------------
%% @doc Handles incomming messages and decides where to pass it on
%%
%%      Data - The message
%%
%%      Socket - The socket form which the message came
%% @end
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
%% @doc Takes a message and formats it into a string ready to be
%%      printed in the client.
%%
%%      FromParser - The message to be manipulated
%%
%%      Sock - The socket form which the message came
%%
%%      List - List containing all rooms and the sockets in each room
%% @end
%% ------------------------------------------------------------------
get_string(FromParser, Sock, List) ->
    Room = string:sub_word(FromParser, 1),
    Len = string:len(Room),
    Msg = string:substr(FromParser, Len+1),
    Name = room:find_name(Sock, List),
    NameMsg = string:join([Room, string:concat(Name, ">"), Msg], " "),
    NameMsg.
