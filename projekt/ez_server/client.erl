-module(client).
-export([client/0]).

client(Server_Address) ->
    Server_Address ! {request, self()},
    receive
        {hit_count,Number} ->
            io:format("CLIENT ~w: Hit count was ~w~n",[self(), Number])
    end.
