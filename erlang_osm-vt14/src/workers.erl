-module(workers).

-export([spawn_worker/4]).

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
	Child_1 = spawn(fun() -> spawn_helper(My_PID,A,B,0)end),
	Child_2 = spawn(fun() -> spawn_helper(My_PID,A,B,1)end),
	receive_Loop(PID,Child_1,Child_2,nill,nill).

spawn_helper(PID,A,B,C) ->
	X = utils:add_values(A,B,C),         
    PID ! {X,C}.

receive_Loop(PID,Child_0,Child_1,Carry_0_Result,Carry_1_Result) ->
	receive
		{Result,0} ->
			receive_Loop(PID,Child_0,Child_1,Result,Carry_1_Result);
		
		{Result,1} ->
			receive_Loop(PID,Child_0,Child_1,Carry_0_Result,Result);
		
		0 when Carry_0_Result /= nill ->
			PID ! element(2,Carry_0_Result),
			exit(Child_1,not_needed),
			element(1,Carry_0_Result);

		1 when Carry_1_Result /= nill ->
			PID ! element(2,Carry_0_Result),
			exit(Child_1,not_needed),
			element(1,Carry_0_Result);

		true ->
		receive_Loop(PID,Child_0,Child_1,Carry_0_Result,Carry_1_Result)

	end.

