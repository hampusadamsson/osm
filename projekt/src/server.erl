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

-export([start_link/0, send/2, return_state/0, list_users/0, crash_me/1]).

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

init(_Args) ->
    process_flag(trap_exit, true),
    tcp_handler:start(1337),
    %server_sup:add_child(),

    case storage_handler:recover_state() of
        {error, _Reason} ->
            io:format("Startup state: Empty\n"),
            Arg = [];
        State ->
            io:format("Startup state:~p \n",[State]),
            
                                                %Arg = [],
            Arg = State,
            storage_handler:delete_state()
    end,
    io:format("Server  running on PID:~w \n",[self()]),
    {ok, Arg}.

%% Establish a Socket to an incoming connection
%% Sock = inc. Socket
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
    spawn(tcp_handler, send_msg,[Msg, Sock, Room, List]),
    {noreply, List};

%% ------------------------------------------------------------------
%% @doc
%% Returns users in a room.
%% @end
%% ------------------------------------------------------------------
handle_cast({'list_room_users', Room, NewList}, _) ->
    spawn(tcp_handler, send_list, [Room, NewList, 1]),
    {noreply, NewList};

%% ------------------------------------------------------------------
%% @doc
%% Returns all the rooms in List.
%% @end 
%% ------------------------------------------------------------------
handle_cast({'list_rooms', NewList}, _) ->
    spawn(tcp_handler, send_list, ["global", NewList, 2]),
    {noreply, NewList};

%% ------------------------------------------------------------------
%% @doc
%% Returns the ip of a user.
%% @end
%% ------------------------------------------------------------------
handle_cast({'whois', Name, Sock}, List) ->
    tcp_handler:send_ip(Name, Sock, List);

%% ------------------------------------------------------------------
%% @doc
%% Returns a list of the rooms that user Name is a member of.
%% @end
%% ------------------------------------------------------------------
handle_cast({'track', Name, Sock}, List) ->
    gen_tcp:send(Sock, room:rooms(List, Name)),
    {noreply, List};    

%% ------------------------------------------------------------------
%% @doc
%% Show info about the room
%% @end
%% ------------------------------------------------------------------
handle_cast({'info', Room, Sock}, List) ->
    gen_tcp:send(Sock, room:get_info(Room, List)),
    {noreply, List};

%% ------------------------------------------------------------------
%% @doc
%% Rename yourself
%% @end
%% ------------------------------------------------------------------
handle_cast({'rename_user', New, Sock}, List) ->
    {noreply, room:rename_user(New, Sock, List)}.

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
handle_call({'return_state'}, _From, List) ->
    {reply, List, List}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    Sockets2kill = room:receivers("global", State, 1),
    lists:foreach(fun(X)->gen_tcp:close(X) end, Sockets2kill),
    
    {ok, Path} = file:get_cwd(),
    storage_handler:save_state(State),

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

return_state()->
    gen_server:call(server, {'return_state'}).

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
    
    
