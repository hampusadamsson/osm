-module(tcp_handler).

-export([start/1, server/1]).

start(LPort) ->
    case gen_tcp:listen(LPort,[{active, false},{packet, line}]) of % 2=line
        {ok, ListenSock} ->
            server:start_servers(ListenSock),
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error,Reason} ->
            {error,Reason}
    end.

server(LS) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            {ok, Data} = gen_tcp:recv(S, 0),
            Length = string:len(Data),
            Name = string:substr(Data, 1, Length-1),
            gen_server:cast(server, {'init_socket', "global", S, Name}),        %%Add to the list 
            server:start_servers(LS),
            loop(S),
            server(LS);
        Other ->
            io:format("accept returned ~w - goodbye!~n",[Other])
    end.


loop(S) ->
    inet:setopts(S,[{active,false}]),
    case gen_tcp:recv(S,0) of
        {ok,Data} ->
            io:format("Msg: ~s \n",[Data]),
            parser:handle(Data, S),
            loop(S);
        {error,Reason} ->
            io:format("Disconnect: ~s \n",[Reason]),
            gen_server:cast(server, {'remove', S}), %global byts mot alla
            gen_tcp:close(S)
    end.

