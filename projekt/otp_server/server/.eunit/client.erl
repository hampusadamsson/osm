-module(client).
-export([client/0, server/0]).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").


client() ->
    Host = "192.168.1.3", % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(Host, 55678, 
                                 [binary, {packet, 0}]),
    ok = gen_tcp:send(Sock, "Some Data"),
    ok = gen_tcp:close(Sock).
    

server() ->
    {ok, LSock} = gen_tcp:listen(55678, [binary, {packet, 0}, 
                                        {active, false}]),
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("ACCEPTED\n"),
    {ok, Bin} = do_recv(Sock, []),
    ok = gen_tcp:close(Sock),
    Bin.

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EUnit adds the fifo:test() function to this module. 

%% All functions with names ending wiht _test() or _test_() will be
%% called automatically by fifo:test()

% new_test_() ->
%     [?_assertEqual(["karl","peter","max","tom"],server:list_user())].
