%%
%% Usage:
%% 4> c(hello_world).
%% {ok,hello_world}
%% 5> hello_world:hello().
%% hello world
%% ok
6>
-module(hello_world).
-export([hello/0]).

hello() ->
	io:format("hello world~n", []).
