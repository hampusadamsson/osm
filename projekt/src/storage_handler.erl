-module(storage_handler).
%% ------------------------------------------------------------------
%% To use EUnit we must include this:
%% ------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

-export([recover_state/0, save_state/1, delete_state/0, empty_rooms/2]).

empty_rooms([], Ack)->
    lists:reverse(Ack);
empty_rooms([{Name, _, Secrecy}|T],Ack)->
    empty_rooms(T, [{Name,[],Secrecy}|Ack]). 

%% ------------------------------------------------------------------
%% @doc Deletes our saved state
%% @end 
%% ------------------------------------------------------------------
delete_state()->
    {ok, Path} = file:get_cwd(),
    file:delete(Path++"/saved_state").

%% ------------------------------------------------------------------
%% @doc (MARKED) Saves the state of the room and users to disk
%%
%%      State - The State 
%% @end 
%% ------------------------------------------------------------------
save_state(State)->
    {ok, Path} = file:get_cwd(),
    NewState=empty_rooms(State,[]),
    file:write_file(Path++"/saved_state",io_lib:fwrite('~p.',[NewState])).

%% ------------------------------------------------------------------
%% @doc Recovers the state of rooms and users from disk
%% @end 
%% ------------------------------------------------------------------
recover_state()->
    {ok, Path} = file:get_cwd(),
    case readlines(Path++"/saved_state") of
        {ok, Tmp}->
            {ok,Tokens,_}=erl_scan:string(Tmp),
            {ok,Term}=erl_parse:parse_term(Tokens),
            Term;
        {error, Reason}->
            {error, Reason}
    end.
                       
%% ------------------------------------------------------------------
%% @doc Reads a whole file from disk and returns as a string 
%%
%%      FileName - The file to read
%% @end 
%% ------------------------------------------------------------------
readlines(FileName) ->
    case  file:open(FileName, [read])  of
        {ok, Device} ->
            Ans=try get_all_lines(Device)
    after file:close(Device)
          end,
    {ok, Ans};
{error, Reason} ->
    {error, Reason}
end.

%% ------------------------------------------------------------------
%% @doc Returns all lines in Device as a string.
%%
%%      Device - a open file
%% @end
%% ------------------------------------------------------------------
get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ get_all_lines(Device)
    end.
