-module(hello_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {'AName', {'AModule', start_link, []},
          Restart, Shutdown, Type, ['AModule']},

{ok,                      % ok, supervisor here's what we want you to do
  {                       
    {                     % Global supervisor options
      one_for_one,        % - use the one-for-one restart strategy
      1000,               % - and allow a maximum of 1000 restarts
      3600                % - per hour for each child process
    },                     
    [                     % The list of child processes you should supervise
      {                   % We only have one
        hello_server,     % - Register it under the name hello_server
        {                 % - Here's how to find and start this child's code 
          hello_server,   %   * the module is called hello_server
          start_link,     %   * the function to invoke is called start_link
          []              %   * and here's the list of default parameters to use
        },                
        permanent,        % - child should run permantenly, restart on crash 
        2000,             % - give child 2 sec to clean up on system stop, then kill 
        worker,            % - FYI, this child is a worker, not a supervisor
        [hello_server]    % - these are the modules the process uses  
      } 
    ]                     
  }                        
}.    

