-module(parser).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

-export([handle/2]).

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
                    gen_server:cast(server, {'remove_socket', Socket});
                
                _ ->
                    gen_server:cast(server, {'send', Room1, Data})   
            end;
       true ->
            gen_server:cast(server, {'send', Room1, Data})
    end.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rooms_test_() ->
ok.


    
