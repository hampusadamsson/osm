-module(server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

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

%% @doc
%% Calls function init_sock in module room
%% @end
%% ------------------------------------------------------------------
handle_cast({'init_socket', Room, NewSock, Name}, List) ->
    {noreply, room:init_sock(Room, List, NewSock, Name)};

%% ------------------------------------------------------------------
%% @doc
%% Calls function add_socket in module room
%% @end
%% ------------------------------------------------------------------
handle_cast({'add_socket', Room, NewSock, Secrecy}, List) ->
    {noreply, room:add_socket(NewSock, Room, List, Secrecy)};

%% ------------------------------------------------------------------
%% @doc
%% Calls function invite in module room
%% @end
%% ------------------------------------------------------------------
handle_cast({'invite', Name, Room}, List) ->
    {noreply, room:invite(Name, Room, List)};

%% ------------------------------------------------------------------
%% @doc
%% Calls function remove_from_all in module room
%% @end
%% ------------------------------------------------------------------
handle_cast({'remove', RemSocket}, List) ->
    {noreply, room:remove_from_all(List, RemSocket)};

%% ------------------------------------------------------------------
%% @doc
%% Calls function remove in module room
%% @end
%% ------------------------------------------------------------------
handle_cast({'remove_from_room', Room, RemSocket}, List) ->
    {noreply, room:remove(Room, List, RemSocket)};

%% ------------------------------------------------------------------
%% @doc
%% Calls function send_msg in module tcp_handler
%% @end
%% ------------------------------------------------------------------
handle_cast({'send', Room, Msg, Sock}, List) ->
    spawn(tcp_handler, send_msg,[Msg, Sock, Room, List]),
    {noreply, List};

%% ------------------------------------------------------------------
%% @doc
%% Calls function send_list in module tcp_handler
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
%% Calls function send_ip in module tcp_handler
%% @end
%% ------------------------------------------------------------------
handle_cast({'whois', Name, Sock}, List) ->
    tcp_handler:send_ip(Name, Sock, List);

%% ------------------------------------------------------------------
%% @doc
%% Calls function rooms in module room
%% @end
%% ------------------------------------------------------------------
handle_cast({'track', Name, Sock}, List) ->
    gen_tcp:send(Sock, room:rooms(List, Name)),
    {noreply, List};    

%% ------------------------------------------------------------------
%% @doc
%% Calls function get_info in module room
%% @end
%% ------------------------------------------------------------------
handle_cast({'info', Room, Sock}, List) ->
    gen_tcp:send(Sock, room:get_info(Room, List)),
    {noreply, List};

%% ------------------------------------------------------------------
%% @doc
%% Calls function rename_user in module room
%% @end
%% ------------------------------------------------------------------
handle_cast({'rename_user', New, Sock}, List) ->
    {noreply, room:rename_user(New, Sock, List)}.

%% ------------------------------------------------------------------
%% @doc
%% Displays information regarding connection, rooms, sockets, processes and users
%% @end
%% ------------------------------------------------------------------
handle_call({'list_users'}, _From, List) ->
    io:format("~s \n",[inet:i()]),
    io:format("Rooms: ~w\n",[length([List])]),
    {reply, List, List};

%% ------------------------------------------------------------------
%% @doc
%% Returns the state of the gen_server
%% @end
%% ------------------------------------------------------------------
handle_call({'return_state'}, _From, List) ->
    {reply, List, List}.

handle_info(_Info, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% @doc
%% This function will activate when the gen_server shuts down; saving state, error log and close socket connections
%% @end
%% ------------------------------------------------------------------
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
%% Unused fuctions, debugging purposes 
%% ------------------------------------------------------------------


%% ------------------------------------------------------------------
%% @doc
%% Sends a message to a room on the gen_server
%% @end
%% ------------------------------------------------------------------
send(Room, Msg)->
    gen_server:cast(server, {'send', Room, Msg}).

%% ------------------------------------------------------------------
%% @doc
%% Returns the state of the gen_server
%% @end
%% ------------------------------------------------------------------
return_state()->
    gen_server:call(server, {'return_state'}).

%% ------------------------------------------------------------------
%% @doc
%% debugging function; Displays information regarding connection, rooms, sockets, processes and users
%% @end
%% ------------------------------------------------------------------
list_users()->
    gen_server:call(server, {'list_users'}).

%% ------------------------------------------------------------------
%% @doc
%% Generates a crash; used to debug the supervisor
%% @end
%% ------------------------------------------------------------------
crash_me(Crash_type)->
    case Crash_type of
        1 ->
            gen_server:call(server, {'fsdadsfeasdfsagsadgagd'});        
        2 ->
            gen_server:call(server, {crash, "i'm crashing"})
    end.
