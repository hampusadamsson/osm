%%%-------------------------------------------------------------------
%%% File    : server.erl
%%% Author  : root <root@ergo>
%%% Description : 
%%%
%%% Created : 23 Apr 2014 by root <root@ergo>
%%%-------------------------------------------------------------------
-module(server).
-export([server/1]).

server(State) ->
    receive
        {request,Return_PID} ->
            io:format("SERVER ~w: Client request received from ~w~n",
                      [self(), Return_PID]) ,
            NewState = State + 1,
            Return_PID ! {hit_count,NewState},
            1server(NewState)
    end.

