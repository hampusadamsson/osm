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

-export([start_link/0, send/2, start_servers/0, send_to_all/2, list_users/0, crash_me/1]).

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
    process_flag(trap_exit, true),
    tcp_handler:start(1337),
    {ok, Args}.

%% Establish a Socket to an incoming connection
%% Sock = inc. Socket
%% ------------------------------------------------------------------
handle_cast({'init_socket', Room, NewSock, Name}, List) ->
    {noreply, room:initSock(Room, List, NewSock, Name)};

%% ------------------------------------------------------------------
%% Add socket with name when writing /join
%% Sock = inc. Socket
%% ------------------------------------------------------------------
handle_cast({'add_socket', Room, NewSock, Secrecy}, List) ->
    {noreply, room:add_socket(NewSock, Room, List, Secrecy)};

%% ------------------------------------------------------------------
%% Invite user Name to room Room
%% ------------------------------------------------------------------
handle_cast({'invite', Name, Room}, List) ->
    {noreply, room:invite(Name, Room, List)};

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
    spawn(?MODULE, send_to_all,[NameMsg, room:receivers(Room, List, 1)]),
    {noreply, List};

%% ------------------------------------------------------------------
%% Returns users in a room.
%% ------------------------------------------------------------------

handle_cast({'list_room_users', Room},List) ->
    send_to_all(room:users_in_room(Room,List), room:receivers(Room,List,1)),
    {noreply, List}.

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
    io:format("Rooms: ~w\n",[length([Sock])]),
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
    {reply, tcp_handler:start(Port), Socket}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Sockets2kill = room:receivers("global", State, 1),
    io:format("Trap exits...\n"),
    lists:foreach(fun(X)->gen_tcp:close(X) end, Sockets2kill),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


send(Room, Msg)->
    gen_server:cast(server, {'send', Room, Msg}).

start_servers()->
    gen_server:call(server, {'start_servers'}).

list_users()->
    gen_server:call(server, {'list_users'}).

crash_me(Crash_type)->
    case Crash_type of
        1 ->
            gen_server:call(server, {'fsdadsfeasdfsagsadgagd'});        
        2 ->
             gen_server:call(server, {crash, "i'm crashing"});
        3  ->
            13/0
        end.

    
%
%-
%----------------------------------------- SERVER-tcp/ip ----
%
%

send_to_all(_,[])->
    ok;
send_to_all(Msg,[Sock|Rest])->
    gen_tcp:send(Sock, Msg),
    send_to_all(Msg,Rest).

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
    
    
