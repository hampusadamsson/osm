-module(server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%%
%%
%%   Run with: server_sup:start_link().
%%              server:start_servers().
%%              
%%   Client:    comment line 171
%%             **this will render the server a client instead
%%
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, connect/2, send/2, start_servers/0, send_to_all/2, list_users/0, print/0]).

%% ------------------------------------------------------------------
%% TCP/IP Sockets Exports
%% ------------------------------------------------------------------
-export([start/1, start_servers/1, server/1, loop/1]). 

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/2, handle_call/3, handle_cast/2, handle_info/2,
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
    {ok, Sock} = gen_tcp:connect(IP, Port, [binary, {active,true}, {packet, line}]), % 2-line kan behöva bytas 
    spawn(?MODULE,loop,[Sock]),
    {noreply, [Sock|_Sock]};

%% ------------------------------------------------------------------
%% Establish a Socket to an incoming connection
%% Sock = inc. Socket
%% ------------------------------------------------------------------
handle_cast({'init_socket', Room, New_Socket, Name}, Sock) ->
    {noreply, room:initSock(Room, Sock, New_Socket, Name)};

%% ------------------------------------------------------------------
%% Add socket with name when writing /join
%% Sock = inc. Socket
%% ------------------------------------------------------------------
handle_cast({'add_socket', Room, New_Socket, Secrecy}, Sock) ->
    Name = room:findName(New_Socket, Sock),
    case lists:keyfind(Room, 1, Sock) of
        {_, SockList, _} ->
            case lists:keyfind(Name, 2, SockList) of
                false ->
                    {noreply, room:insert(Room, Sock, New_Socket, Name, Secrecy)};
                _ ->
                    {noreply, Sock}
            end;
        false ->
            {noreply, room:insert(Room, Sock, New_Socket, Name, Secrecy)}
    end;

%% ------------------------------------------------------------------
%% Invite user Name to room Room
%% ------------------------------------------------------------------
handle_cast({'invite', Name, Room}, List) ->
    case room:findSock(Name, List) of
        false ->
            {noreply, List};
        Sock ->
            {noreply, room:insert(Room, List, Sock, Name, true)}
    end;

%% ------------------------------------------------------------------
%% Remove socket from list after disconnect
%% Rem_Sock = The one to remove
%% ------------------------------------------------------------------
handle_cast({'remove', Rem_Socket}, Sock) ->
    {noreply, room:removeFromAll(Sock, Rem_Socket)};

%% ------------------------------------------------------------------
%% Remove socket from certain room in list
%% Rem_Sock = The one to remove
%% ------------------------------------------------------------------
handle_cast({'remove_from_room', Room, Rem_Socket}, Sock) ->
    {noreply, room:remove(Room, Sock, Rem_Socket)};

%% ------------------------------------------------------------------
%% Sends a message !IF! connected
%% Sock = socket created by 'connect'
%% ------------------------------------------------------------------
handle_cast({'send', Room, Msg, Sock}, List) ->
    NameMsg = parser:getString(Msg, Sock, List),
    send_to_all(NameMsg, room:receivers(Room, List, 1)),
    {noreply, List};

%% ------------------------------------------------------------------
%% Returns users in a room.
%% ------------------------------------------------------------------
handle_cast({'list_room_users', Room},Sock) ->
    send_to_all(room:users_in_room(Room,Sock), room:receivers(Room,Sock)),
    {noreply, Sock}.


%% ------------------------------------------------------------------
%% Find name connected to Sock
%% ------------------------------------------------------------------
handle_call({'find_name', Socket}, AllRooms) ->
    {reply, room:findName(Socket, AllRooms), AllRooms}. 

%% ------------------------------------------------------------------
%% Displays current user-list
%% ------------------------------------------------------------------
handle_call({'list_users'}, _From, Sock) ->
    io:format("~s \n",[inet:i()]),
    io:format("Connections: ~w\n",[length(Sock)]),
    {reply, Sock, Sock};

%% ------------------------------------------------------------------
%% Listen for incoming connections 
%%
%% start(Arg1,Arg2)
%% Arg1 = listening port for incoming servers
%%
%% ------------------------------------------------------------------
handle_call({'start_servers'}, _From, Socket) ->
    Port=1337,
    {reply, start(Port), Socket}.

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

send(Room, Msg)->
    gen_server:cast(server, {'send', Room, Msg}).

start_servers()->
    gen_server:call(server, {'start_servers'}).

list_users()->
    gen_server:call(server, {'list_users'}).

print()->
io:format("HAMPUS\n").

%
%-
%----------------------------------------- SERVER-tcp/ip ----
%
%

%%                 TODO
%              
%   ok      lyssnande servern
%   ok      listan på sockets - update
%           fixa bra print - inet:i().       


send_to_all(_,[])->
    ok;
send_to_all(Msg,[Sock|Rest])->
    gen_tcp:send(Sock, Msg),
    send_to_all(Msg,Rest).

start(LPort) ->
    case gen_tcp:listen(LPort,[{active, false},{packet, line}]) of % 2=line
        {ok, ListenSock} ->
            start_servers(ListenSock),
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error,Reason} ->
            {error,Reason}
    end.

start_servers(LS) ->
    spawn(?MODULE,server,[LS]).

server(LS) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            {ok, Data} = gen_tcp:recv(S, 0),
            Length = string:len(Data),
            Name = string:substr(Data, 1, Length-1),
            gen_server:cast(server, {'init_socket', "global", S, Name}),        %%Add to the list 
            start_servers(LS),
            loop(S),
            server(LS);
        Other ->
            io:format("accept returned ~w - goodbye!~n",[Other]),
            start_servers(LS)
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %% EUnit adds the fifo:test() function to this module. 

% %% All functions with names ending wiht _test() or _test_() will be
% %% called automatically by fifo:test()

% new_test_() ->
% %% Check that server havent been started
%     A=whereis(server),                   
%     _Tmp = ?_assertEqual(undefined,A),
    
% %% Start server and check existans
%     server:start_link(),
%     B=whereis(server),
%     ?_assertNotEqual(undefined,B).
    

% start_servers_test_() ->
% %% Starting the servers
%     server:start_servers(),
    
%     _A=lists:seq(1,150),
%     lists:foreach(fun(_X)->server:connect(localhost,1337) end, _A),
    
%     ?_assertEqual(1,1). %%This port (1337) may come to change
    
    
