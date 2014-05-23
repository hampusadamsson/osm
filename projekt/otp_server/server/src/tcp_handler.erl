-module(tcp_handler).

-export([start/1, server/1]).

start(LPort) ->
<<<<<<< HEAD
        io:format("Socket listening: ~w ~n",[self()]),
    case gen_tcp:listen(LPort,[{active, false},{packet, line},{reuseaddr, true}]) of % 2=line
        {ok, ListenSock} ->

            Tmp = spawn(tcp_handler, server,[ListenSock]), %<---- supervisor needed (Låt genserver skapa dessa ???)
            io:format("New Connectiv: ~w ~n",[Tmp]),

=======
    case gen_tcp:listen(LPort,[{active, false},{packet, line}]) of % 2=line
        {ok, ListenSock} ->
            server:start_servers(ListenSock),
>>>>>>> Erik
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error,Reason} ->
            {error,Reason}
<<<<<<< HEAD
    end. 
=======
    end.
>>>>>>> Erik

server(LS) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            {ok, Data} = gen_tcp:recv(S, 0),
            Length = string:len(Data),
            Name = string:substr(Data, 1, Length-1),
<<<<<<< HEAD
            gen_server:cast(server, {'init_socket', "global", S, Name}),

            Tmp = spawn(tcp_handler, server,[LS]), %<---- supervisor needed
            io:format("New Connectiv: ~w ~n",[Tmp]),

=======
            gen_server:cast(server, {'init_socket', "global", S, Name}),        %%Add to the list 
            server:start_servers(LS),
>>>>>>> Erik
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

