%%%-------------------------------------------------------------------
%%% File    : server_sup.erl
%%% Author  : root <root@ergo>
%%% Description : 
%%%
%%% Created : 25 Apr 2014 by root <root@ergo>
%%%-------------------------------------------------------------------
-module(server_sup).

-behaviour(supervisor).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         start_link/0
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
init([]) ->
    AChild = {'server',{'server',start_link,[]},
              permanent,2000,worker,['server']},
    {ok,                      % ok, supervisor here's what we want you to do
  {                       
    {                     % Global supervisor options
      one_for_one,        % - use the one-for-one restart strategy
      1000,               % - and allow a maximum of 1000 restarts
      3600                % - per hour for each child process
    },                     
    [                     % The list of child processes you should supervise
      {                   % We only have one
        'server',     % - Register it under the name hello_server
        {                 % - Here's how to find and start this child's code 
          'server',   %   * the module is called hello_server
          start_link,     %   * the function to invoke is called start_link
          []              %   * and here's the list of default parameters to use
        },                
        permanent,        % - child should run permantenly, restart on crash 
        2000,             % - give child 2 sec to clean up on system stop, then kill 
        worker,            % - FYI, this child is a worker, not a supervisor
        ['server']    % - these are the modules the process uses  
      } 
    ]                     
  }                        
}.

%%====================================================================
%% Internal functions
%%====================================================================
