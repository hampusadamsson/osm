-module(server).
-export([server/0]).

    server() ->
    {ok, LSock} = gen_tcp:listen(1337, [binary, {packet, 0}, 
                                        {active, false}]),
    {ok, Sock} = gen_tcp:accept(LSock),				%%Ska vara denna
    {ok, Bin} = do_recv(Sock, []),
    gen_tcp:send(Sock,Bin),
    gen_tcp:close(Sock),
    gen_tcp:close(LSock),
    List = binary_to_list(Bin),
    io:fwrite("~p",[List]),
    server().

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.