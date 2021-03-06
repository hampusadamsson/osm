%% @doc Erlang mini project.
-module(add).
-export([start/3, start/6]).

%% @doc Calculates the addtion between two 
%%      integers A and B in base Base. The sum is represented 
%%		in base 10.
%%
%% === Example ===      
% <div class="example">```
%% > add:start(123,14322,10).
%% 14445'''
%% </div>

-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(A,B,Base) ->
 
	start(A,B,Base,3,specOff,{0,1}).


%% @doc Calculates the addtion between two 
%%      integers A and B in base Base, with 
%%      the option which splits the list 
%%      in Option segments. The sum is represented 
%%		in base 10.
%%   
%% === Example ===      
% <div class="example">```
%% > add:start(123,14322,10,3).
%% 14445'''
%% </div>

-spec start(A,B,Base,Split,Specc,Sleep) -> ok when 
      A::integer(),
      Split::integer(), 
      B::integer(), 
      Specc::atom(),
      Base::integer(),
      Sleep::integer() | tuple(),
      Split::[Split].

start(Aa,Bb,Base,Split,Specc,Sleep) ->
    A = list_to_integer(Aa, Base),
    B = list_to_integer(Bb, Base),
    La=utils:intlist(A),
    Lb=utils:intlist(B),
    {ListA, ListB} = utils:fulfill(La,Lb),
    
    if
        Split>length(La) -> 
            erlang:error('Cant split a list into more elements than the number of chars inserted'); 
        true ->
            
            SplitA=utils:split(ListA,Split),
            SplitB=utils:split(ListB,Split),
            Tmp = (utils:getSum(SplitA,SplitB,Specc,Sleep)),
            Tmp2 = utils:list_to_int(Tmp),
            io:fwrite("~s + ~s = ~s \n", [Aa,Bb,integer_to_list(Tmp2, Base)])                   
    end.

%%_____________________________________________________________________

%% %% add_all([],[]) ->
%% %%     [];
%% %% add_all([HeadA|A],[HeadB|B]) ->
%% %%     Tmp = add_values(HeadA,HeadB,0),
%% %%     [Tmp|add_all(A,B)].
%% add_all([[]],[[]]) ->
%%     [[]];
%% add_all(A,B) ->
%%     My_Pid = self(),
%%     spawn(fun()->spawn_worker(My_Pid,A,B)end),    
%%     receive
%%         {X,0} ->
%%             X;
%%         {X,1} ->
%%             lists:concat([[1],X])
%%     end.
    
%% spawn_worker(PID, [A|[]],[B|[]]) ->
%%     X = add_values((A),(B),0),         
%%     PID ! X;    

%% spawn_worker(PID,[HeadA|A],[HeadB|B]) ->
%%     MyPid = self(),
%%     Child = spawn(fun()->spawn_worker(MyPid,A,B)end),
%%     Sum0 = add_values(HeadA,HeadB,0),
%%     Sum1 = add_values(HeadA,HeadB,1),
%%     receive
%%         {X,0} ->
%%             PID ! {lists:concat([element(1,Sum0),X]),element(2,Sum0)};
%%         {X,1} ->
%%             PID ! {lists:concat([element(1,Sum1),X]),element(2,Sum1)}
%%         end.

%% list_to_int([]) ->
%%     0;
%% list_to_int(L) ->
%%     Tmp = lists:map(fun(X) -> X+48 end, L),
%%     list_to_integer(Tmp).

%% %% add_help(ListA, ListB) ->
%% %%     Sum = list_to_int(ListA) + list_to_int(ListB),
%% %%     SumList = intlist(Sum),
%% %%     [H|T] = intlist(Sum),
%% %%     if
%% %%         length([H|T]) > length(ListA) ->
%% %%             {T,1};
%% %%         true ->
%% %%             {SumList, 0}
%% %%     end.
   
%% make_same(A,B) ->
%%     if
%%         length(A) > length(B) ->
%%             make_same(A,[0|B]);
%%             true ->
%%             B
%%     end.

%% add_values(A,B,C) ->
%%     Tmp=list_to_int(A)+list_to_int(B)+C,
%%     Len_sum = intlist(Tmp),

%%     if
%%         (length(Len_sum)>length(A)) ->
%%             [_Xx|Tail]=Len_sum,
%%             Tmp2=list_to_int(Tail),
%%             %%{Tmp2,1};
%%             {Tail,1};
%%         (length(Len_sum)<length(A)) ->
%%             {make_same(A,Len_sum),0};
%%         true ->
%%             {Len_sum,0}
%%     end.

%% to_base_10(A,Base) ->
%%     Tmp = lists:mapfoldr(fun(X,Sum) -> {X*Sum,Sum*Base} end, 1, A),
%%     Tmp2 = element(1,Tmp),
%%     Tmp3 = lists:foldl(fun(Y, Sum2) -> Y + Sum2 end, 0, Tmp2),
%%     intlist(Tmp3).

%% intlist(A) ->
%%     Tmp = integer_to_list(A),
%%     lists:map(fun(X)-> X-48 end,Tmp).
    
%% fulfill(A,B) ->
%%     if 
%%         length(A)>length(B) ->
%%             fulfill(A, [0|B]);
%%         length(B)>length(A) ->
%%             fulfill([0|A] ,B);
%%         true ->
%%             {A,B}
%%     end.

%% split(L, N) when length(L) < N ->
%%     L;

%% split(L, Nn) ->
%%     Nn2=(length(L)/Nn),
%%     N=round(Nn2),
%%     if Nn == 1 ->
%%             [L];
%%        true ->
%%             split(L, N, [])
%%     end.

%% split(L, N, Lists) ->
%%     {L1, L2} = lists:split(N, L),
%%     if length(L2) > N ->
%% 	    split(L2, N, [L1|Lists]);
%%        true ->
%%             lists:reverse([L2, L1|Lists])
%%     end.
