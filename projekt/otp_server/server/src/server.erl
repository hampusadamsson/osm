-module(server).
-behaviour(gen_server).
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
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, connect/2, send/1, start_servers/0, send_to_all/2, list_users/0]).

%% ------------------------------------------------------------------
%% TCP/IP Sockets Exports
%% ------------------------------------------------------------------
-export([start/2, start_servers/2, server/1, loop/1]). 

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

%% ------------------------------------------------------------------
%% Connects to a remote Host (Not. a server function)
%% IP = remote ip
%% Port = remote port
%% ------------------------------------------------------------------
handle_cast({'connect', IP, Port}, _Sock) ->
    {ok, Sock} = gen_tcp:connect(IP, Port, [binary, {active,false}, {packet, 2}]),
    spawn(loop(Sock)),
    {noreply, [Sock|_Sock]};

%% ------------------------------------------------------------------
%% Establish a Socket to an incoming connection
%% Sock = inc. Socket
%% ------------------------------------------------------------------
handle_cast({'add_socket', New_Socket}, Sock) ->
    {noreply, [New_Socket|Sock]};

%% ------------------------------------------------------------------
%% Listen for incoming connections 
%%
%% start(Arg1,Arg2)
%% Arg1 = numbers of servers listening
%% Arg2 = listening port for incoming servers
%%
%% ------------------------------------------------------------------
handle_cast({'start_servers'},Socket) ->
    start(10,1337),
    {noreply, Socket};

%% ------------------------------------------------------------------
%% Sends a message !IF! connected
%% Sock = socket created by 'connect'
%% ------------------------------------------------------------------
handle_cast({'send', Msg},Sock) ->
    send_to_all(Msg, Sock),    %%gen_tcp:send(Sock, Msg),
    {noreply, Sock}.

%% ------------------------------------------------------------------
%% Displays current user-list
%% ------------------------------------------------------------------
handle_call({'list_users'},_From, Sock) ->
    {reply, Sock, Sock}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


connect(IP,Port)->
    gen_server:cast(server, {'connect', IP, Port}).

send(Msg)->
    gen_server:cast(server, {'send', Msg}).

start_servers()->
    gen_server:cast(server, {'start_servers'}).

list_users()->
    gen_server:call(server, {'list_users'}).


%
%-
%----------------------------------------- SERVER-tcp/ip ----
%-
%
send_to_all(_,[])->
    ok;
send_to_all(Msg,[Sock|Rest])->
    %%  spawn(?MODULE,send_to_all,[Msg,[Rest]]),
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
    inet:setopts(S,[{active,once}]),
    receive
        {tcp,S,Data} ->
%%            Answer = process(Data),
            Answer = Data,
            gen_tcp:send(S,Answer),
            io:format("Msg: ~s ~n",[Data]),
            loop(S);
        {tcp_closed,S} ->
            io:format("Socket ~w closed [~w]~n",[S,self()]),
            ok
    end.
