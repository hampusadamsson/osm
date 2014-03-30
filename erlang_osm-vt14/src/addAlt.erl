%% @doc Erlang mini project.
-module(addAlt).
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
            Tmp = (utilsAlt:getSum(SplitA,SplitB,Specc,Sleep)),
            Tmp2 = utils:list_to_int(Tmp),
            io:fwrite("~s + ~s = ~s \n", [Aa,Bb,integer_to_list(Tmp2, Base)])                   
    end.