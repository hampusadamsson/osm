-module(client).
-export([start/2, start_servers/2, server/1, loop/1, connect/2, send/2]).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

start(Num,LPort) ->
    case gen_tcp:listen(LPort,[{active, false},{packet,2}]) of
        {ok, ListenSock} ->
            start_servers(Num,ListenSock),
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error,Reason} ->
            {error,Reason}
    end.

start_servers(0,_) ->
    ok;

start_servers(Num,LS) ->
    spawn(?MODULE,server,[LS]),
    start_servers(Num-1,LS).

server(LS) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            loop(S),
            server(LS);
        Other ->
            io:format("accept returned ~w - goodbye!~n",[Other]),
            ok
    end.

loop(S) ->
    inet:setopts(S,[{active,false}]),
    {ok,Data} = gen_tcp:recv(S,0),
    io:format("Msg: ~s \n",[Data]),
    loop(S).
    % receive
%         {tcp,S,Data} ->
% %%            Answer = process(Data),
%             Answer = Data,
%             gen_tcp:send(S,Answer),
%             io:format("Msg: ~s ~n",[Data]),
%             loop(S);
%         {tcp_closed,S} ->
%             io:format("Socket ~w closed [~w]~n",[S,self()]),
%             ok
%     end.

%%
%%
%%
%%
%%
%%A simple client

%%connect(IP,PortNo) ->
connect(IP, Port) ->
    %%IP = "localhost",
    %%Port = 1337,
    {ok,Sock} = gen_tcp:connect(IP,Port,[{active,true},{packet,2}]),
    spawn(?MODULE,loop,[Sock]),
    Sock.
    % gen_tcp:send(Sock,Message),
    % A = gen_tcp:recv(Sock,0),
    % gen_tcp:close(Sock),
    % io:format("message: + ~s\n",[A]),
    % A.

send(Sock, Message) ->
    gen_tcp:send(Sock,Message).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EUnit adds the fifo:test() function to this module. 

%% All functions with names ending wiht _test() or _test_() will be
%% called automatically by fifo:test()

% new_test_() ->
%     [?_assertEqual(["karl","peter","max","tom"],server:list_user())].
