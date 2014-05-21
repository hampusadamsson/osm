-module(parser).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

-export([handle/2, getString/3]).

%% ------------------------------------------------------------------
%% Incomming Data - Data being a string
%%
%% Data will be parsed to execute functions
%%
%% ------------------------------------------------------------------
handle(Data, Socket)->   
    [Room|Body] = string:tokens(Data, " "),
    %%Msg=string:substr(Data, length(Room)+1, length(Data)),
    Room1=string:concat(Room,""),
    
    if length(Body)>1 ->
        [Request|[Namn|_]]=Body,
        [Namn2] = string:tokens(Namn,"\n"),
        Namn1=string:concat(Namn2,""),
        
        case Request of
            "/join" ->
                gen_server:cast(server, {'add_socket', Namn1, Socket});

            "/exit" ->
                gen_server:cast(server, {'remove_from_room', Namn1, Socket});
            
            _ ->
                gen_server:cast(server, {'send', Room1, Data, Socket})   
        end;
    true ->
        Request = Body,
        case Request of
            "/exit" ->
                gen_server:cast(server, {'remove', Socket});
            _ ->
                gen_server:cast(server, {'send', Room1, Data, Socket})   
        end
    end.

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


    
