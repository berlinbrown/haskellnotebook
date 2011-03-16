%%
%% Simple tuple test.
%%
%%$ erl
%% Eshell V5.6  (abort with ^G)
%% 1> c(tupl).
%% {ok,tupl}
%% 3> tupl:convert_length({inch, 4}).
%% {centimeter,10.1600}

-module(tupl).
-export([convert_length/1]).

convert_length({centimeter, X}) ->
    {inch, X / 2.54};

convert_length({inch, Y}) ->
    {centimeter, Y * 2.54}.
