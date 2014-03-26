-module(workers).

-export([spawn_worker/3,createProcessList/3,completeTest/1]).

% spawn_worker(A,B,specOff) ->

% 	Sum_0 = utils:add_values(A,B,0),
%     Sum_1 = utils:add_values(A,B,1),
%     receive
%     	{0,Element,List} ->
%          	{Result,Carry} = Sum_0,
%          	if
% 				Element < length(List) ->
% 					lists:nth(Element,List) ! {Carry,Element+1,List},
% 					PID ! Result;
% 				true ->
% 					if
% 						Carry == 0 ->
% 							PID ! Result;
% 						true ->
% 							PID ! [1]++Result
% 						end
% 					end;

			
%        {1,Element,List} ->
%            {Result,Carry} = Sum_1,
%          	if
% 				Element < length(List) ->
% 					lists:nth(Element,List) ! {Carry,Element+1,List},
% 					PID ! Result;
% 				true ->
% 					if
% 						Carry == 0 ->
% 							PID ! Result;
% 						true ->
% 							PID ! [1]++Result
% 					end
% 			end	
		
			
% 	end;

spawn_worker(A,B,specOn) ->
	
	My_PID = self(),
	Child_1 = spawn(fun() -> spawn_helper(My_PID,A,B,0)end),
	Child_2 = spawn(fun() -> spawn_helper(My_PID,A,B,1)end),
	receive_Loop(Child_1,Child_2,nill,nill).

spawn_helper(PID,A,B,C) ->
	X = utils:add_values(A,B,C),         
    PID ! {X,C}.

receive_Loop(Child_0,Child_1,Carry_0_Result,Carry_1_Result) ->
	receive
		{Result,0} ->
			receive_Loop(Child_0,Child_1,Result,Carry_1_Result);
		
		{Result,1} ->
			receive_Loop(Child_0,Child_1,Carry_0_Result,Result);
		
		{0,PID} when Carry_0_Result /= nill ->
			PID ! Carry_0_Result,
			exit(Child_1,not_needed);
			

		{1,PID} when Carry_1_Result /= nill ->
			PID ! Carry_1_Result,					
			exit(Child_0,not_needed);

		true ->
		receive_Loop(Child_0,Child_1,Carry_0_Result,Carry_1_Result)

	end.



createProcessList(L1,L2,Specc) ->
	%PID = [self()],
	Mode = [Specc],
	FixedList = [[A,B] ||{A,B} <- lists:zip(L1,L2)],
	[spawn(fun() -> spawn_worker(B,C,D)end) || [B,C] <- FixedList, D <- Mode].



completeTest(Specc) ->
	L1 = [[8,1],[1,2],[3,3]],
	L2 = [[1,3],[2,2],[2,3]],
	L3 = lists:reverse(L1),
	L4 = lists:reverse(L2),
 	ProcessList = createProcessList(L3,L4,Specc),
 	{X,Y} = recursiveGet(0,ProcessList,[]),
 	if
 		Y == 0 ->
 			X;
 		true ->
 			[1]++X
 	end.


recursiveGet(Carry,[],SumList) ->
	{SumList,Carry};
recursiveGet(Carry,[Head|Tail],SumList) ->
	My_PID = self(),
	Head ! {Carry, My_PID},
	receive
		{X,Y} ->	
			recursiveGet(Y,Tail,X++SumList)
	end.
