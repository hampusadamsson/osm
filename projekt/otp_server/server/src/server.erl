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

-export([start_link/0, add_user/1, list_user/0, connect/2, send/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {[],[]}, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.


%% ------------------------------------------------------------------
%% Adds users to the looping-list
%% ------------------------------------------------------------------
handle_cast({'add_user', Name},{State,_Sock}) ->
    {noreply, {[Name|State], _Sock}};

%% ------------------------------------------------------------------
%% Connects to a remote Host 
%% IP = remote ip
%% Port = remote port
%% ------------------------------------------------------------------
handle_cast({'connect', IP, Port},{State,_Sock}) ->
    {ok, Sock} = gen_tcp:connect(IP, Port, [binary, {packet, 0}]),
    {noreply, {State, Sock}};

%% ------------------------------------------------------------------
%% Sends a message !IF! connected
%% Sock = socket created by 'connect'
%% ------------------------------------------------------------------
handle_cast({'send', Msg},{State,Sock}) ->
    ok = gen_tcp:send(Sock, Msg),
%%    ok = gen_tcp:close(Sock),
    {noreply, {State, Sock}}.

%% ------------------------------------------------------------------
%% Displays current user-list
%% ------------------------------------------------------------------
handle_call({'list_user'},_From, {State,_Sock}) ->
    {reply, State, {State, _Sock}}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

add_user(Name)->
    gen_server:cast(server, {'add_user', Name}).

list_user()->
    gen_server:call(server, {'list_user'}).

connect(IP,Port)->
    gen_server:cast(server, {'connect', IP, Port}).

send(Msg)->
    gen_server:cast(server, {'send', Msg}).
