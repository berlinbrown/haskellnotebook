%%
%% Euler 25:
%% Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.
%% What is the first term in the Fibonacci sequence to contain 1000 digits?
%%

-module(euler25).
-export([main/0]).

fib(2) -> 1;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).

fib_str(N) ->
	X = fib(N),
	lists:flatten(io_lib:format("~p", [X])).

loop(100) -> io:format("done~n");
loop(N) ->
	Z = fib_str(N),
	X = length(Z),
	io:format("ans=>~p, n:~p~n", [X, Z]),
	loop(N+1).

main() ->
	io:format("Euler 25~n"),
	loop(1),
	io:format("Done~n").

%% End of File
