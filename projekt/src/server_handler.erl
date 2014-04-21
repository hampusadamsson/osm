-module(server_handler).
-export([new_user_list/0, size/1, add_user/2, remove_user/1, empty/1,users/1]).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

%%NEW USER LIST 
new_user_list() -> {users, []}.

%%USER LIST LENGTH
size({users, In}) ->
    length(In).

%%LIST ALL USERS
users({users, In}) -> 
    {users, In}.

%%LÄGG TILL USER
add_user({users, In}, X) -> 
    {users, [X|In]}.

%%TA BORT USER
remove_user({users, []}) -> 
    erlang:error('empty_user-list');
remove_user({users, [H|T]}) -> 
    {H, {users, T}}.


%% KONTROLLERAR TOM USER LIST
empty({users, []}) ->
    true;
empty({users, _}) -> 
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EUnit adds the fifo:test() function to this module. 

%% All functions with names ending wiht _test() or _test_() will be
%% called automatically by fifo:test()


new_test_() -> 
    [?_assertEqual({users, []}, new_user_list()), 
     ?_assertMatch(0, server_handler:size(new_user_list())),
     ?_assertException(error, 'empty_user-list', remove_user(new_user_list()))].

add_user_test() ->
    add_user(new_user_list(), a).

add_user_remove_user_test() ->
    ?assertMatch({a,_}, remove_user(add_user(new_user_list(), a))).


f1() ->
    add_user(add_user(add_user(new_user_list(), foo), bar), "Ahloa!").

size_test_() ->
    F1 = f1(),
    F2 = add_user(F1, atom),
    {_, F3} = server_handler:remove_user(F2),

    [?_assertMatch(3, server_handler:size(F1)),
     ?_assertMatch(4, server_handler:size(F2)),
     ?_assertMatch(3, server_handler:size(F3))].

    
add_user_test_() ->
    F1 = f1(),
    F2 = add_user(f1(), last),
    
    [ ?_assertMatch(1, server_handler:size(server_handler:add_user(server_handler:new_user_list(), a))),
      ?_assertEqual(server_handler:size(F1) + 1, server_handler:size(F2))]. 

empty_test_() ->
    F = f1(),
    {_, F2} = remove_user(F),
    {_, F3} = remove_user(F2),
    {_, F4} = remove_user(F3),
    
    [?_assertMatch(true, empty(new_user_list())),
     ?_assertMatch(false, empty(F)),
     ?_assertMatch(false, empty(F2)),
     ?_assertMatch(false, empty(F3)),
     ?_assertMatch(true, empty(F4))].
    

