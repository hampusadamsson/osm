-module(server).
-export([start/3, start/6]).

-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(A,B,Base) ->
	start(A,B,Base,3,specOff,{0,1}).

start(Aa,Bb,Base,Split,Specc,Sleep) ->
                    
