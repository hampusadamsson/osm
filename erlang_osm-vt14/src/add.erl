%% @doc Erlang mini project.
-module(add).
-export([start/3, add_values/3, start/4, to_base_10/2, intlist/1, fulfill/2]).

%% @doc TODO: add documentation
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(A,B, Base) ->
    tbi.

%% @doc TODO: add documentation
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(A,B,Base, Options) ->
    tbi.


%%_____________________________________________________________________

%% repeat(Char,N) ->
%%     [Char || _ <- lists:seq(1,N)].

add_values(A,B,C) ->
    ListA = intlist(A),
    ListB = intlist(B),
    Tmp=A+B,
    Len_sum = intlist(Tmp),
    if
        (length(Len_sum)>length(ListA)) ->
            {Tmp,1};
        true ->
            {Tmp,0}
    end.

to_base_10(A,Base) ->
    Tmp = lists:mapfoldr(fun(X,Sum) -> {X*Sum,Sum*Base} end, 1, A),
    Tmp2 = element(1,Tmp),
    Tmp3 = lists:foldl(fun(Y, Sum2) -> Y + Sum2 end, 0, Tmp2),
    intlist(Tmp3).

intlist(A) ->
    Tmp = integer_to_list(A),
    lists:map(fun(X)-> X-48 end,Tmp).
    
fulfill(A,B) ->
    if 
        length(A)>length(B) ->
            fulfill(A, [0|B]);
        length(B)>length(A) ->
            fulfill([0|A] ,B);
        true ->
            {A,B}
    end.
