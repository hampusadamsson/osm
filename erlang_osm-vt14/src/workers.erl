-module(workers).

-export([spawn_worker/4,createProcessList/3,createTestList/0, getResults/0]).

spawn_worker(PID,A,B,specOff) ->

	Sum_0 = utils:add_values(A,B,0),
    Sum_1 = utils:add_values(A,B,1),
    receive
    	0 ->
         	{Result,Y} = Sum_0,
         	PID ! Y,
         	Result;
        1 ->
           {Result,Y} = Sum_1,
         	PID ! Y,
         	Result
	end;

spawn_worker(PID,A,B,specOn) ->
	
	My_PID = self(),
	Child_1 = spawn(fun() -> spawn_helper(A,B,0)end),
	Child_2 = spawn(fun() -> spawn_helper(A,B,1)end),
	receive_Loop(Child_1,Child_2,nill,nill).

spawn_helper(A,B,C) ->
	X = utils:add_values(A,B,C),         
    PID ! {X,C}.

receive_Loop(Child_0,Child_1,Carry_0_Result,Carry_1_Result) ->
	receive
		{Result,0} ->
			receive_Loop(Child_0,Child_1,Result,Carry_1_Result);
		
		{Result,1} ->
			receive_Loop(Child_0,Child_1,Carry_0_Result,Result);
		
		{0,Element,List} when Carry_0_Result /= nill ->
			PID ! element(2,Carry_0_Result),
			exit(Child_1,not_needed),
			element(1,Carry_0_Result);

		{1,Element,List} when Carry_1_Result /= nill ->
			if
				Element > length(List) ->
					body
			end
			PID ! element(2,Carry_0_Result),
			exit(Child_1,not_needed),
			element(1,Carry_0_Result);

		true ->
		receive_Loop(Child_0,Child_1,Carry_0_Result,Carry_1_Result)

	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

createProcessList(L1,L2,Specc) ->
	PID = [self()],
	Mode = [Specc],
	FixedList = [[A,B] ||{A,B} <- lists:zip(L1,L2)].
	[spawn(fun() -> spawn_worker(B,C,D)end) || [B,C] <- FixedList, D <- Mode].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resultTest(X) ->
	receive 
		{give_result,PID} ->
			PID ! X
	end.

createTestList() ->
	L1 = ['a','b','c','d','e'],
	[spawn(fun() -> resultTest(X)end) || X <- L1].

getResults() ->
	L1 = createTestList(),
	P = fun(A,AccIn) -> lists:concat([sendMessage(A), AccIn]) end,
	lists:foldl(P,'',L1).  


sendMessage(PID) ->
	OwnPid = self(),
	PID ! {give_result, OwnPid},
	receive
		X ->
			X
		end.