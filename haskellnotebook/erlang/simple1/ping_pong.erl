%%
%% Usage:
%% 22> c(ping_pong).
%% {ok,ping_pong}
%% 23> ping_pong:start().
%% Pong received ping
%% <0.99.0>
%% Ping received pong
%% 24> Pong received ping
%% 24> Ping received pong
%% 24> Pong received ping
%% 24> Ping received pong
%% 24> ping finished
%% 24> Pong finished
%%
%% Quote from erlang docs on processes.
%% http://www.erlang.org/doc/getting_started/conc_prog.html#3
%%
%% "Messages between Erlang processes are simply valid Erlang terms. 
%% I.e. they can be lists, tuples, integers, atoms, pids etc.
%% Each process has its own input queue for messages it receives. 
%% New messages received are put at the end of the queue. 
%% When a process executes a receive, the first message in the queue is matched against the first pattern in the receive, if this matches, the message is removed from the queue and the actions corresponding to the the pattern are executed.
%% However, if the first pattern does not match, the second 
%% pattern is tested, if this matches the message is removed from the queue and the actions corresponding to the second pattern are executed. If the second pattern does not match the third is tried and so on until there are no more pattern to test. If there are no more patterns to test, the first message is kept in the queue and we try the second message instead. If this matches any pattern, the appropriate actions are executed and the second message is removed from the queue (keeping the first message and any other messages in the queue). If the second message does not match we try the third message and so on until we reach the end of the queue. If we reach the end of the queue, the process blocks (stops execution) and waits until a new message is received and this procedure is repeated.
%% Of course the Erlang implementation is "clever" 
%% and minimizes the number of times each message is tested 
%% against the patterns in each receive."
%% 

-module(ping_pong).

-export([start/0, ping/2, pong/0]).

ping(0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("ping finished~n", []);

ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_PID).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start() ->
    Pong_PID = spawn(ping_pong, pong, []),
    spawn(ping_pong, ping, [3, Pong_PID]).
