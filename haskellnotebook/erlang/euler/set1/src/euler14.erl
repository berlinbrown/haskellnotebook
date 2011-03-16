%%
%% Euler Problem 14 in Erlang
-module(euler14).
-export([main/0]).

iter_seq(N) when ((N rem 2) == 0) -> N / 2;
iter_seq(N)                       -> ((3 * N) + 1).

euler14_loop(N, Idx) ->
	if (N =< 1) -> { N, (Idx+1) };
	   true     ->
			R = iter_seq(round(N)),
			euler14_loop(R, (Idx+1))
	end.

euler14(N, Mx) when (N =< 1) ->
	io:format("Loop done =>max:~p~n", [Mx]);
euler14(N, Mx) ->
	{ _, Ct} = euler14_loop(N, 0),
	NewMax = if Ct > Mx -> io:format("-->~p~n", [N]), Ct; true -> Mx end,
	euler14(N - 1, NewMax).

main() ->
	io:format("Euler Problem~n"),
	euler14(1000000, -1).

%% End of File
