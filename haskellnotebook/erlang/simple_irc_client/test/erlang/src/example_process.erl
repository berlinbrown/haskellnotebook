-module(my).

-export([run_all/0, start/1, go/1]).

start(Data) ->
	spawn_link(?MODULE, go, [Data]).

go(Data) ->
	io:format("running:go~n"),
	proceed("Data"),
	io:format("go:done~n").

proceed(Data) ->
	io:format("[1] proceed~n"),
	receive
		{ launch } ->
			io:format("-> launching!!!~n")			
	end,
	proceed("Data2", pong).

proceed(Self, connecting) ->
	io:format("[2] proceed~n");
proceed(Self, pong) ->
    receive
        { hasdata } ->
			io:format("[3] At has data~n"),
            proceed(Self, connecting)
	end.

run_all() ->
	io:format("running [!]~n"),
	D = "Me",
	P = start(D),
	io:format("My PID~p~n", [P]),	
	timer:sleep(3000),
	P ! { launch },
	timer:sleep(3000),
	P ! { hasdata },
	timer:sleep(4000),
	io:format("done~n").

%% End of the file
 
