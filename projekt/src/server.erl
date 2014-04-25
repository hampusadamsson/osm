%%-module(server). 

%%-export([new/0, size/1, add_user/2, remove_user/1, empty/1, users/1]).

%% To use EUnit we must include this:
%%-include_lib("eunit/include/eunit.hrl").

%%new() ->
%%    spawn(fun() -> loop(server_handler:new_user_list()) end).

%% This is the process loop.

%%loop(User_list) ->
%%    receive 
%%        {size, PID} ->
%%            PID ! {size, server_handler:size(User_list)},
%%            loop(User_list);
%%        {users, PID} ->
%%            PID ! {users, server_handler:users(User_list)},
%%            loop(User_list); 
%%        {empty, PID} ->
%%            PID ! server_handler:empty(User_list),
%%            loop(User_list);
%%        {remove_user, PID} ->
%%            case server_handler:empty(User_list) of
%%                true ->
%%                    PID ! {error, 'empty_user-list'},
%%                    loop(User_list);
%%                false ->
%%                    Tmp = server_handler:remove_user(User_list),
%%                    PID ! Tmp,
%%                        loop(element(2, Tmp))
%%            end;
%%        {add_user, Value, PID} ->
%%            Tmp = server_handler:add_user(User_list, Value),
%%            PID ! Tmp,
%%            loop(Tmp)
%%end.

%%users(User_list) ->
%%    User_list ! {users, self()},
%%    receive 
%%        {users, List} ->
%%            List
%%    end.

%%size(User_list) ->
%%    User_list ! {size, self()},
%%    receive 
%%        {size, Size} ->
%%            Size
%%    end.

%%empty(User_list) ->
%%    User_list ! {empty, self()},
%%    receive 
%%        true ->
%%            true;
%%        false  ->
%%            false
%%    end.


%%remove_user(User_list) ->
%%    User_list ! {remove_user, self()},
%%    receive
%%        {Value, {users, _Out}} ->
%%            Value;
%%        {error, _Msg} ->
%%            {error, _Msg}
%%    end.


%%add_user(User_list, Value) ->
%%    User_list ! {add_user, Value, self()},
%%    receive
%%        {users, In} ->
%%            {users, In}
                
%%    end.

-module(server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3, start_link/0, size/1, empty/1, add_user/2,
        remove_user/1, users/1]).

-include_lib("eunit/include/eunit.hrl").

init(UserList) -> {ok, UserList}.

handle_call({size}, _From, UserList) ->
    {reply, server_handler:size(UserList), UserList};
handle_call({empty}, _From, UserList) ->
    {reply, server_handler:empty(UserList), UserList};
handle_call({add_user, NewUser}, _From, UserList) ->
    {reply, server_handler:add_user(UserList, NewUser), UserList};
handle_call({remove_user}, _From, UserList) ->
    {reply, server_handler:remove_user(UserList), UserList};
handle_call({users}, _From, UserList) ->
    {reply, server_handler:users(UserList), UserList};
handle_call(terminate, _From, UserList) ->
    {stop, normal, ok, UserList}.
 
handle_cast({return, NewUser}, {users, UserList}) ->
    {noreply, {users, [NewUser|UserList]}};
handle_cast({return}, {users, [_|T]}) ->
    {noreply, {users, T}}.

handle_info(Msg, UserList) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, UserList}.

terminate(normal, {users}) ->
    [io:format("EXIT")],
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.

start_link() -> gen_server:start_link(?MODULE, {users, []}, []).

size(UserList) ->
    gen_server:call(UserList, {size}).

empty(UserList) ->
    gen_server:call(UserList, {empty}).

add_user(UserList, NewUser) ->
    gen_server:call(UserList, {add_user, NewUser}),
    gen_server:cast(UserList, {return, NewUser}).

remove_user(UserList) ->
    gen_server:call(UserList, {remove_user}),
    gen_server:cast(UserList, {return}).

users(UserList) ->
    gen_server:call(UserList, {users}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EUnit adds the server_handler:test() function to this module. 

%% All functions with names ending wiht _test() or _test_() will be
%% called automatically by pserver_handler:test()

%%common_test_() ->
%%    {ok, A} = start_link(),
%%    [?_assertMatch(true,empty(A)),
%%     ?_assertMatch({users,[ett]},add_user(A,ett)),
%%     ?_assertMatch({users,[tva,ett]},add_user(A,tva)),
%%     ?_assertMatch({users,[tre,tva,ett]},add_user(A,tre)),
%%     ?_assertMatch(false,empty(A)),
%%     ?_assertMatch(tre,remove_user(A)),
%%     ?_assertMatch(tva,remove_user(A)),
%%     ?_assertMatch(ett,remove_user(A)),
%%     ?_assertMatch(true,empty(A))].

common_test_() ->
    {ok, A} = start_link(),
    [?_assertMatch(true,empty(A)),
     ?_assertMatch(ok,add_user(A,ett)),
     ?_assertMatch(ok,add_user(A,tva)),
     ?_assertMatch(ok,add_user(A,tre)),
     ?_assertMatch({users, [tre, tva, ett]},users(A)),
     ?_assertMatch(false,empty(A)),
     ?_assertMatch(ok,remove_user(A)),
     ?_assertMatch(ok,remove_user(A)),
     ?_assertMatch(ok,remove_user(A)),
     ?_assertMatch(true,empty(A))].

users_test_() ->
    {ok, A} = start_link(),
    [?_assertMatch(true,empty(A)),
     ?_assertMatch({users,[]},users(A)),
     ?_assertMatch(ok,add_user(A,ett)),
     ?_assertMatch({users,[ett]},users(A)),
     ?_assertMatch(ok,add_user(A,tva)),
     ?_assertMatch({users,[tva,ett]},users(A)),
     ?_assertMatch(ok,add_user(A,tre)),
     ?_assertMatch({users,[tre,tva,ett]},users(A))].

%%start_test_() ->
%%    [?_assertMatch(true, is_pid(new())),
%%     ?_assertMatch(0, server:size(new())),
%%     ?_assertMatch(true, empty(new())),
%%     ?_assertMatch({error, 'empty_user-list'}, remove_user(new()))].

empty_test() ->
    {ok, F} = start_link(),
    ?assertMatch(true, empty(F)),
    add_user(F, foo),
    ?assertMatch(false, empty(F)),
    remove_user(F),
    ?assertMatch(true, empty(F)).
		  
add_user_remove_user_test_() ->
    {ok, F} = start_link(),
    add_user(F, foo),
    add_user(F, bar),
    add_user(F, luz),
    [?_assertMatch({users,[luz,bar,foo]},users(F)),
    ?_assertMatch(false, empty(F)),
    ?_assertMatch(ok, remove_user(F)),
    ?_assertMatch(ok, remove_user(F)),
    ?_assertMatch(ok, remove_user(F)),
    ?_assertMatch(true, empty(F))].

		 
    
    
    
    
    
    
    
