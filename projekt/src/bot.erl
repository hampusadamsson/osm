-module(bot).

-export([check_idle/0, start_bot/0, loop/1]).

%
%    Returns all connected sockets
%
all_sockets()->
    State =  gen_server:call(server, {'return_state'}),
    case State of
        [{"global", List, _}|_] ->
            lists:map(fun(X)->element(1,X) end, List);
        _ ->
            []
    end.
   
%
%   
%

check_idle()->
    Sockets = all_sockets(),
    io:format("Sockets: ~w ~n",[Sockets]),
    lists:foreach(fun(X)->spawn(response_test(X)) end, Sockets).

response_test(Socket)->
case Socket of
    ok->
        ok;
    _->
        Msg = "global ping \n",
        gen_tcp:send(Socket, Msg)
end.


  %  {ok, Data} = gen_tcp:recv(Socket,0),
    
start_bot()->
    {ok, Sock}=gen_tcp:connect(localhost,1337,[{active,false},{packet,line}]),
    gen_tcp:send(Sock,"BOT\n"),
    io:format("Bot starting up ~n"),
    spawn(bot, loop, [Sock]).


loop(Sock)->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data}-> 
            bot_parser(Data),
            loop(Sock);
        {error,_Reason} ->
            gen_server:cast(server, {'remove', Sock}),
            gen_tcp:close(Sock)
    end.


bot_parser(Msg)->

    case  0<string:str(Msg,"/time") of 
        true->
            io:format("Time: ~w ~n",[time()]);
        false->
            ok
    end,

    io:format("Bot: ~s ~n",[Msg]).


    
