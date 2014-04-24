-module(localErlang).
-export([start/0,messageHandler/0]).


     
messageHandler() ->
  
	receive
    	
		{_,'\\EXIT'} ->
			io:format("Kommandot funkade också, tally hoooooo!!"),
			messageHandler();
		
		{_,'\\JOIN'} ->
			io:format("Kommandot funkade, woop woop!"),
			messageHandler();
		
		{PingId,A} ->
			PingId ! lists:concat([A]),
			messageHandler()
    end.
 
start() ->
         register(messageHandler,spawn(localErlang,messageHandler,[])),
		 ExtProg = "java -cp .;C:\\OtpErlang.jar GUI",
		 open_port({spawn, ExtProg}, [stream]).