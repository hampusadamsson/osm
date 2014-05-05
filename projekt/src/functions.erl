-module(functions).

-export([send_to_all/2, start/3, start_servers/3, loop/1]).

send_to_all(_,[])->
    ok;
send_to_all(Msg,[Sock|Rest])->
    gen_tcp:send(Sock, Msg),
    send_to_all(Msg,Rest).

start(Num, LPort, Module) ->
    case gen_tcp:listen(LPort,[{active, false},{packet,2}]) of
        {ok, ListenSock} ->
            start_servers(Num, ListenSock, Module),
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error,Reason} ->
            {error,Reason}
    end.

start_servers(0, _, _) ->
    ok;
start_servers(Num, LS, Module) ->
    spawn(?MODULE,server,[LS]),
    start_servers(Num-1, LS, Module).

loop(S) ->
    inet:setopts(S,[{active,false}]),
    {ok,Data} = gen_tcp:recv(S,0),
    io:format("Msg: ~s \n",[Data]),
    %%gen_server:cast(server, {'send', Data}),
    loop(S).

