%% @doc Erlang mini project.
-module(add).
-export([start/3, add_all/2, split/2, add_values/3, start/4, to_base_10/2, intlist/1, fulfill/2]).

%% @doc TODO: add documentation
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(A,B, Base) ->
    La=to_base_10(intlist(A),Base),
    Lb=to_base_10(intlist(B),Base),
    {ListA, ListB} = fulfill(La,Lb),
    Result = add_values(ListA, ListB,0).

%% @doc TODO: add documentation
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(A,B,Base, Options) ->
    La=to_base_10(intlist(A),Base),
    Lb=to_base_10(intlist(B),Base),
    {ListA, ListB} = fulfill(La,Lb),
    SplitA=split(ListA,Options),
    SplitB=split(ListB,Options),
    add_all(SplitA,SplitB).
    %%Result = add_values(ListA, ListB,0).


%%_____________________________________________________________________

add_all([],[]) ->
    [];
%% add_all([HeadA|[]],[HeadB|[]]) ->
%%     [add_values(HeadA,HeadB,0)];
add_all([HeadA|A],[HeadB|B]) ->
    Tmp = add_values(HeadA,HeadB,0),
    [Tmp|add_all(A,B)].
    
list_to_int([]) ->
    0;
list_to_int(L) ->
    Tmp = lists:map(fun(X) -> X+48 end, L),
    list_to_integer(Tmp).

add_values(A,B,C) ->
    Tmp=list_to_int(A)+list_to_int(B),
    Len_sum = intlist(Tmp),
    if
        (length(Len_sum)>length(A)) ->
            [_Xx|Tail]=Len_sum,
            Tmp2=list_to_int(Tail),
            {Tmp2,1};
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

split(L, N) when length(L) < N ->
    L;

split(L, Nn) ->
    Nn2=(length(L)/Nn),
    N=round(Nn2),
    if Nn == 1 ->
            [L];
       true ->
            split(L, N, [])
    end.

split(L, N, Lists) ->
    {L1, L2} = lists:split(N, L),
    if length(L2) > N ->
	    split(L2, N, [L1|Lists]);
       true ->
            lists:reverse([L2, L1|Lists])
    end.
