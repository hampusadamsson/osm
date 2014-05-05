-module(server).

-define(SERVER, ?MODULE).
%%
%%
%%
%%   Run with: server_sup:start_link().
%%              server:add_user("Per").
%%
%%
%%
%% ------------------------------------------------------------------
%% TCP/IP Sockets Exports
%% ------------------------------------------------------------------
-export([start/2, start_servers/2, server/1, loop/1]). 

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    generic:start_link().

%
%-
%----------------------------------------- SERVER-tcp/ip ----
%-
%
send_to_all(_,[])->
    ok;
send_to_all(Msg,[Sock|Rest])->
    gen_tcp:send(Sock, Msg),
    send_to_all(Msg,Rest).

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
            gen_server:cast(server, {'add_socket',S}),        %%Add to the list 
            
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
    %%gen_server:cast(server, {'send', Data}),
    loop(S).
