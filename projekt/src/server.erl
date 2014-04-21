-module(server). 

-export([new/0, size/1, add_user/2, remove_user/1, empty/1, users/1]).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

new() ->
    spawn(fun() -> loop(server_handler:new_user_list()) end).

%% This is the process loop.

loop(User_list) ->
    receive 
        {size, PID} ->
            PID ! {size, server_handler:size(User_list)},
            loop(User_list);
        {users, PID} ->
            PID ! {users, server_handler:users(User_list)},
            loop(User_list); 
        {empty, PID} ->
            PID ! server_handler:empty(User_list),
            loop(User_list);
        {remove_user, PID} ->
            case server_handler:empty(User_list) of
                true ->
                    PID ! {error, 'empty_user-list'},
                    loop(User_list);
                false ->
                    Tmp = server_handler:remove_user(User_list),
                    PID ! Tmp,
                        loop(element(2, Tmp))
            end;
        {add_user, Value, PID} ->
            Tmp = server_handler:add_user(User_list, Value),
            PID ! Tmp,
            loop(Tmp)
end.

users(User_list) ->
    User_list ! {users, self()},
    receive 
        {users, List} ->
            List
    end.

size(User_list) ->
    User_list ! {size, self()},
    receive 
        {size, Size} ->
            Size
    end.

empty(User_list) ->
    User_list ! {empty, self()},
    receive 
        true ->
            true;
        false  ->
            false
    end.


remove_user(User_list) ->
    User_list ! {remove_user, self()},
    receive
        {Value, {users, _Out}} ->
            Value;
        {error, _Msg} ->
            {error, _Msg}
    end.


add_user(User_list, Value) ->
    User_list ! {add_user, Value, self()},
    receive
        {users, In} ->
            {users, In}
                
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EUnit adds the server_handler:test() function to this module. 

%% All functions with names ending wiht _test() or _test_() will be
%% called automatically by pserver_handler:test()

common_test_() ->
    A = new(),
    [?_assertMatch(true,empty(A)),
     ?_assertMatch({users,[ett]},add_user(A,ett)),
     ?_assertMatch({users,[tva,ett]},add_user(A,tva)),
     ?_assertMatch({users,[tre,tva,ett]},add_user(A,tre)),
     ?_assertMatch(false,empty(A)),
     ?_assertMatch(tre,remove_user(A)),
     ?_assertMatch(tva,remove_user(A)),
     ?_assertMatch(ett,remove_user(A)),
     ?_assertMatch(true,empty(A))].

users_test_() ->
    A = new(),
    [?_assertMatch(true,empty(A)),
     ?_assertMatch({users,[]},users(A)),
     ?_assertMatch({users,[ett]},add_user(A,ett)),
     ?_assertMatch({users,[ett]},users(A)),
     ?_assertMatch({users,[tva,ett]},add_user(A,tva)),
     ?_assertMatch({users,[tva,ett]},users(A)),
     ?_assertMatch({users,[tre,tva,ett]},add_user(A,tre)),
     ?_assertMatch({users,[tre,tva,ett]},users(A))].

start_test_() ->
    [?_assertMatch(true, is_pid(new())),
     ?_assertMatch(0, server:size(new())),
     ?_assertMatch(true, empty(new())),
     ?_assertMatch({error, 'empty_user-list'}, remove_user(new()))].

empty_test() ->
    F =  new(),
    ?assertMatch(true, empty(F)),
    add_user(F, foo),
    ?assertMatch(false, empty(F)),
    remove_user(F),
    ?assertMatch(true, empty(F)).
		  
add_user_remove_user_test() ->
    F = new(),
    add_user(F, foo),
    add_user(F, bar),
    add_user(F, luz),
    ?assertMatch(false, empty(F)),
    ?assertMatch(luz, remove_user(F)),
    ?assertMatch(bar, remove_user(F)),
    ?assertMatch(foo, remove_user(F)),
    ?assertMatch({error, 'empty_user-list'}, remove_user(F)).

		 
    
    
    
    
    
    
    
