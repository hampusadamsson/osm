-module(fifo).
-export([new/0, size/1, push/2, pop/1, empty/1]).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").


%%%%%%% Implementation of a FIFO buffer %%%%%%%

%% @doc Creates an empty FIFO buffer. 
-opaque fifo()::{fifo, list(), list()}.
-spec new() -> fifo().

%% Represent the FIFO using a 3-tuple {fifo, In, Out} where In and
%% Outs are lists. 

new() -> {fifo, [], []}.
 
%% @doc Returns the number of elements in Fifo.
-spec size(Fifo) -> integer() when
      Fifo::fifo().

size({fifo, In, Out}) ->
    length(In) + length(Out).

%% @doc Returns Fifo with X pushed onto it.
%% TODO: add a -spec type declaration
-spec push(Fifo, X) -> fifo() when
    Fifo::fifo(),
    X::any().

%% To make it fast to push new values, add a new value to the head of
%% In.

push({fifo, [], Y }, X) -> 
    {fifo, [X], Y };
push({fifo, In, Out}, X) -> 
    {fifo, [X|In], Out}.

%% @doc Pops Fifo.
%% @throws 'empty fifo'
%% TODO: add a -spec type declaration
-spec pop(Fifo) -> fifo() when
    Fifo::{any(), fifo()}.

%% pop should return {Value, NewFifo}

pop({fifo, [], []}) -> 
    erlang:error('empty fifo');

%% To make pop fast we want to pop of the head of the Out list.
    
pop({fifo, In, [H|T]}) -> 
    {H, {fifo, In, T}};

%% When Out is empty, we must take a performance penalty. Use the
%% reverse of In as the new Out and an empty lists as the new In, then
%% pop as usual.

pop({fifo, In, []}) ->
    F = {fifo, [], lists:reverse(In)},
    pop(F).


%% @doc Returns true if Fifo is empty, otherwise false.
-spec empty(Fifo) -> boolean() when Fifo::fifo().

empty({fifo, [], []}) ->
    true;
empty({fifo, _, _}) -> 
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EUnit adds the fifo:test() function to this module. 

%% All functions with names ending wiht _test() or _test_() will be
%% called automatically by fifo:test()


new_test_() -> 
    [?_assertEqual({fifo, [], []}, new()), 
     ?_assertMatch(0, fifo:size(new())),
     ?_assertException(error, 'empty fifo', pop(new()))].

push_test() ->
    push(new(), a).

push_pop_test() ->
    ?assertMatch({a,_}, pop(push(new(), a))).


f1() ->
    push(push(push(new(), foo), bar), "Ahloa!").

size_test_() ->
    F1 = f1(),
    F2 = push(F1, atom),
    {_, F3} = fifo:pop(F2),

    [?_assertMatch(3, fifo:size(F1)),
     ?_assertMatch(4, fifo:size(F2)),
     ?_assertMatch(3, fifo:size(F3))].

    
push_test_() ->
    F1 = f1(),
    F2 = push(f1(), last),
    
    [ ?_assertMatch(1, fifo:size(fifo:push(fifo:new(), a))),
      ?_assertEqual(fifo:size(F1) + 1, fifo:size(F2))]. 

empty_test_() ->
    F = f1(),
    {_, F2} = pop(F),
    {_, F3} = pop(F2),
    {_, F4} = pop(F3),
    
    [?_assertMatch(true, empty(new())),
     ?_assertMatch(false, empty(F)),
     ?_assertMatch(false, empty(F2)),
     ?_assertMatch(false, empty(F3)),
     ?_assertMatch(true, empty(F4))].
    
