%%
%% Simple factorial in erlang
%%

-module(fact).
-export([fact/1]).

fact(1) ->
    1;
fact(N) ->
    N * fact(N - 1).

