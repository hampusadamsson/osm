-module(room).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

-export([remove/3, insert/3, receivers/2]).

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% returns a list of sockets in a room
% 
% ex. [{room_name, [sock1,sock2,sock3]}]
%
%--------------------------------------------------------------------------
receivers(Room, List) ->
    case lists:keyfind(Room, 1, List) of
        {Room, Sock_List}->
            New_List=Sock_List;
        false ->
            New_List = []
    end,
    New_List.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% insert into the List of rooms
% 
% ex. [{room_name, [sock1,sock2,sock3]}]
%
%--------------------------------------------------------------------------

insert(Room, List, Socket)->
    case lists:keyfind(Room, 1, List) of
        {Room, Sock_List}->
            Tmp_List = lists:keydelete(Room, 1, List),
            New_List = [{Room, [Socket|Sock_List]}|Tmp_List];
    
        false ->
            New_List = [{Room, [Socket]}|List]
    end,
    New_List.

%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% remove the List of rooms
% 
% ex. [{room_name, [sock1,sock2,sock3]}]
%
%--------------------------------------------------------------------------
remove(Room, List, Socket)->
    case lists:keyfind(Room, 1, List) of
        {Room, Sock_List}->
            Tmp_List = lists:keydelete(Room, 1, List),
            Sock_List2 = lists:delete(Socket, Sock_List),
            if 
                length(Sock_List2)==0 ->
                    New_List = Tmp_List;
                true ->  
                    New_List = [{Room, Sock_List2}|Tmp_List]
            end;
            false ->
                                 New_List = List
                         end,
    New_List.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rooms_test_() ->
    A = [],
    B = insert(rum1, A, s1),
    _=?_assertEqual(B,[{rum1,[s1]}]),
        
    C = insert(rum2, B, s2),
    __=?_assertEqual(C,[{rum2,[s2]},{rum1,[s1]}]),

    D = insert(rum1, C, s3),
    ___=?_assertEqual(D,[{rum1,[s3,s1]},{rum2,[s2]}]),

    E1 = remove(rum123, D, s1),
    ____=?_assertEqual(E1,[{rum1,[s3,s1]},{rum2,[s2]}]),
    
    ____1=?_assertEqual(receivers(rum1,E1),[s3,s1]), 
    ____2=?_assertEqual(receivers(rumqwe1,E1),[]), 

    E = remove(rum1, D, s1),
    _____=?_assertEqual(E,[{rum1,[s3]},{rum2,[s2]}]),

    F = remove(rum1, E, s3),
    ______=?_assertEqual(F,[{rum2,[s2]}]),

    G = remove(rum2, F, s2),
    _______=?_assertEqual(G,[]).
    
