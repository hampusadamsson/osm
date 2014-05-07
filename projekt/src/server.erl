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
%%   Client:                
%%
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start_servers/0, send_to_all/2, list_users/0]).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [{"Global", []}], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

%% ------------------------------------------------------------------
%% Sends a message !IF! connected
%% Sock = socket created by 'connect'
%% ------------------------------------------------------------------
handle_cast({'send', Room, Sock, Msg}, AllRooms1) ->
    SockList = room:findRoom(AllRooms1, Room),
    if
        SockList =:= [] ->
            AllRooms2 = room:addToRoom(Room, Sock, AllRooms1, AllRooms1),
            NewSockList = room:findRoom(AllRooms2, Room),
            send_to_all(Msg, NewSockList),
            {noreply, AllRooms2};
        true ->
            case lists:member(Sock, SockList) of
                true ->
                    send_to_all(Msg, SockList),
                    {noreply, AllRooms1};
                false ->
                    send_error({Sock, "You can't enter this room without an invite~n"}),
                    {noreply, AllRooms1}
            end
    end;

%% ------------------------------------------------------------------
%% Sends a message !IF! connected
%% Sock = socket created by 'connect'
%% ------------------------------------------------------------------
handle_cast({'add_to_room', RoomName, Sock}, AllRooms) ->
    {noreply, room:addToRoom(RoomName, Sock, AllRooms, AllRooms)}.

%% ------------------------------------------------------------------
%% Listen for incoming connections 
%%
%% start(Arg1,Arg2)
%% Arg1 = numbers of servers listening
%% Arg2 = listening port for incoming servers
%%
%% ------------------------------------------------------------------
handle_call({'start_servers'}, _From, Socket) ->
    Port=1337,
    {reply, start(10, Port), Socket};

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

start_servers()->
    gen_server:call(server, {'start_servers'}).

list_users()->
    gen_server:call(server, {'list_users'}).


%
%-
%----------------------------------------- SERVER-tcp/ip ----
%
%

%%                 TODO
%              
%         lyssnande servern
%         listan på sockets - update
%         


send_to_all(_,[])->
    ok;
send_to_all(Msg,[Sock|Rest])->
    gen_tcp:send(Sock, Msg),
    send_to_all(Msg,Rest).

send_error({Sock, Reason}) ->
    gen_tcp:send(Sock, Reason).

start(Num,LPort) ->
    case gen_tcp:listen(LPort,[{active, false},{packet,line}]) of
        {ok, ListenSock} ->
            start_servers(Num,ListenSock),
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error,Reason} ->
            {error,Reason}
    end.

start_servers(0, _) ->
    ok;
start_servers(Num,LS) ->
    spawn(?MODULE,server,[LS]),
    start_servers(Num-1,LS).

server(LS) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            gen_server:cast(server, {'add_to_room', "Global", S}),        %%Add to the list 
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
    Room = room:getRoomName(Data),
    Length = string:len(Room),
    Msg = string:substr(Data, Length+2),
    gen_server:cast(server, {'send', Room, S, Msg}),
    loop(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EUnit adds the fifo:test() function to this module. 

%% All functions with names ending wiht _test() or _test_() will be
%% called automatically by fifo:test()

new_test_() ->
%% Check that server havent been started
    A=whereis(server),                   
    _Tmp = ?_assertEqual(undefined,A),
    
%% Start server and check existans
    server:start_link(),
    B=whereis(server),
    ?_assertNotEqual(undefined,B).
    

start_servers_test_() ->
%% Starting the servers
    server:start_servers(),
    
    A=lists:seq(1,10),
    lists:foreach(fun(_X)->server:connect(localhost,1337) end, A),

    ?_assertEqual(1,1). %%This port (1337) may come to change
    
    
