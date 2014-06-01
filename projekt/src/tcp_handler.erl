-module(tcp_handler).

-export([start/1, server/1, send_ip/3, send_to_all/2]).

%% ---------------------------------------------------------------------------
%% @doc (MARKED) Starts listening to incoming connections to the server??
%%
%%      LPort - The port to listen to
%% @end
%% ---------------------------------------------------------------------------
start(LPort) ->
    io:format("Socket listening: ~w ~n",[self()]),
    case gen_tcp:listen(LPort,[{active, false},{packet, line},{reuseaddr, true}]) of % 2=line
        {ok, ListenSock} ->
            Tmp = spawn(tcp_handler, server,[ListenSock]), %<---- supervisor needed (Låt genserver skapa dessa ???)
            io:format("New Connectiv: ~w ~n",[Tmp]),
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error,Reason} ->
            {error,Reason}
    end. 

%% ---------------------------------------------------------------------------
%% @doc Accepts and establishes a connection with the client
%%
%%      LS - The socket trying to establish a connection
%% @end
%% ---------------------------------------------------------------------------
server(LS) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            {ok, Data} = gen_tcp:recv(S, 0),
            Length = string:len(Data),
            Name = string:substr(Data, 1, Length-1),
            gen_server:cast(server, {'init_socket', "global", S, Name}),
            Tmp = spawn(tcp_handler, server,[LS]), %<---- supervisor needed
            io:format("New Connectiv: ~w ~n",[Tmp]),
            loop(S),
            server(LS);
        Other ->
            io:format("accept returned ~w - goodbye!~n",[Other])
    end.

%% ---------------------------------------------------------------------------
%% @doc A loop reciving messages form a socket
%%
%%      S - A socket
%% @end
%% ---------------------------------------------------------------------------
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

%% ---------------------------------------------------------------------------
%% @doc Sends a message with ip and port of a user Name to a socket
%%
%%      Name - The username of the user to send info about
%%
%%      Sock - The socket to send the message to
%%
%%      List - List containing all rooms and the sockets in each room
%% @end
%% ---------------------------------------------------------------------------
send_ip(Name, Sock, List) ->
    case room:get_ip(Name, List) of
        false ->
            {noreply, List};    
        {Ip, Port} ->
            Msg = io_lib:format("{whois User: ~s,Connectd From: ~s,On port: ~w}~n",[Name, Ip, Port]),
            gen_tcp:send(Sock, Msg),
            {noreply, List}    
    end.

%% ---------------------------------------------------------------------------
%% @doc Sends a message Msg to al list of sockets Sock
%%
%%      Msg - A message in the form of a string
%%
%%      Sock - List of sockets
%% @end
%% ---------------------------------------------------------------------------
send_to_all(_,[])->
    ok;
send_to_all(Msg,[Sock|Rest])->
    gen_tcp:send(Sock, Msg),
    send_to_all(Msg,Rest).
