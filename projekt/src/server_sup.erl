%%%-------------------------------------------------------------------
%%% File    : server_sup.erl
%%% Author  : root <root@ergo>
%%% Description : 
%%%
%%% Created : 28 Apr 2014 by root <root@ergo>
%%%-------------------------------------------------------------------
-module(server_sup).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

-behaviour(supervisor).

-define(CHILD(I, Type), {I, {I, start_servers, []}, permanent, 5000, Type, [I]}).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         start_link/0, add_child/1
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
         init/1
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init(Arg) ->
    Child = {'server',{'server',start_link, Arg},
              permanent,2000,worker,['server']},
      
    _One_for_one = {one_for_one,500,60},
    _One_for_all = {one_for_all,500,60},
    _Rest_for_one = {rest_for_one,500,60},
    _Simple_one_for_one = {simple_one_for_one,500,60},
 
    Restart_strategy = _One_for_one,
        
      {ok,{Restart_strategy, [Child]}}.


%%--------------------------------------------------------------------
%% Func: add_child/1
%%--------------------------------------------------------------------
add_child(Sock)->
    % supervisor:start_child(supervisor_mod,
    %                        {example_proc, {example_proc, start_link, []},
    %                         permanent, brutal_kill, worker, [example_proc]}).
    supervisor:start_child(?MODULE, {accepter, {tcp_handler, server, [Sock]}, permanent, 2000, worker, dynamic}). 


%%====================================================================
%% Internal functions
%%====================================================================




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EUnit adds the fifo:test() function to this module. 

%% All functions with names ending wiht _test() or _test_() will be
%% called automatically by fifo:test()

% new_test_() ->
%     server_sup:start_link(),
%     server:add_user("tom"),
%     server:add_user("max"),
%     server:add_user("peter"),
%     server:add_user("karl"),

%     [?_assertEqual(["karl","peter","max","tom"],server:list_user())].



