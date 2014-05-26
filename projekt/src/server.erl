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
    process_flag(trap_exit, true),
    tcp_handler:start(1337),
    {ok, Args}.

%% ------------------------------------------------------------------
%% @doc
%% Establish a Socket to an incoming connection
%% Sock = inc. Socket
%% @end
%% ------------------------------------------------------------------
handle_cast({'init_socket', Room, NewSock, Name}, List) ->
    {noreply, room:init_sock(Room, List, NewSock, Name)};

%% ------------------------------------------------------------------
%% @doc
%% Add socket with name when writing /join
%% Sock = inc. Socket
%% @end
%% ------------------------------------------------------------------
handle_cast({'add_socket', Room, NewSock, Secrecy}, List) ->
    {noreply, room:add_socket(NewSock, Room, List, Secrecy)};

%% ------------------------------------------------------------------
%% @doc
%% Invite user Name to room Room
%% @end
%% ------------------------------------------------------------------
handle_cast({'invite', Name, Room}, List) ->
    {noreply, room:invite(Name, Room, List)};

%% ------------------------------------------------------------------
%% @doc
%% Remove socket from list after disconnect
%% Rem_Sock = The one to remove
%% @end
%% ------------------------------------------------------------------
handle_cast({'remove', RemSocket}, List) ->
    {noreply, room:remove_from_all(List, RemSocket)};

%% ------------------------------------------------------------------
%% @doc
%% Remove socket from certain room in list
%% Rem_Sock = The one to remove
%% @end
%% ------------------------------------------------------------------
handle_cast({'remove_from_room', Room, RemSocket}, List) ->
    {noreply, room:remove(Room, List, RemSocket)};

%% ------------------------------------------------------------------
%% @doc
%% Sends a message !IF! connected
%% Sock = socket created by 'connect'
%% @end
%% ------------------------------------------------------------------
handle_cast({'send', Room, Msg, Sock}, List) ->
    NameMsg = parser:get_string(Msg, Sock, List),
    spawn(?MODULE, send_to_all,[NameMsg, room:receivers(Room, List, 1)]),
    {noreply, List};

%% ------------------------------------------------------------------
%% @doc
%% Returns users in a room.
%% @end
%% ------------------------------------------------------------------
handle_cast({'list_room_users', Room},List) ->
    Rooms = room:users_in_room(Room, List),
    Receivers = room:receivers(Room, List, 1),
    spawn(?MODULE, send_to_all, [Rooms, Receivers]),
    {noreply, List}.

%% ------------------------------------------------------------------
%% @doc
%% Displays current user-list
%% @end
%% ------------------------------------------------------------------
handle_call({'list_users'}, _From, List) ->
    io:format("~s \n",[inet:i()]),
    io:format("Rooms: ~w\n",[length([List])]),
    {reply, List, List};

%% ------------------------------------------------------------------
%% @doc
%% Listen for incoming connections 
%%
%% start(Arg1,Arg2)
%% Arg1 = listening port for incoming servers
%% @end
%% ------------------------------------------------------------------
handle_call({'start_servers'}, _From, Socket) ->
    Port=1337,
    {reply, tcp_handler:start(Port), Socket}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    Sockets2kill = room:receivers("global", State, 1),
    io:format("Trap exits...\n"),
    lists:foreach(fun(X)->gen_tcp:close(X) end, Sockets2kill),
    
    {ok, Path} = file:get_cwd(),

    file:write_file(Path++"/crash_dump",
                    io_lib:fwrite("\>>STATE when server terminated:\n~p\n\n>>Connected SOCKETS when server terminated:\n~p\n\nREASON for termination:\n~p",[State, Sockets2kill, Reason])),
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
            gen_server:call(server, {crash, "i'm crashing"})
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
    
    
