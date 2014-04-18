-module(whisper).
-export([start/1]).

start(I) ->
    start(self(),I-1).

start(Pred,  0) ->
    Pred ! "Go!",
    receive
        X ->
            io:format("Done!\n")
    end;

start(Pred,I) ->
    Next = spawn(fun() -> func(Pred)end),
    start(Next,I-1).

func(To) ->
    receive
        X ->
            To ! X
    end.
