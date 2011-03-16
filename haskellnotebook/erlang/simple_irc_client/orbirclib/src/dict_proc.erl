-module(dict_proc).
-author("orbitz@ezabel.com").

-export([start/0, start/1, stop/1, loop/1, fetch/2, has_key/2, store/3]).

start() ->
    start(dict:new()).

start(Dict) ->
    spawn_link(?MODULE, loop, [Dict]).

stop(Pid) ->
    Pid ! {stop, self()},
    ok.
        

loop(Dict) ->
    receive
        {fetch, Pid, Key} ->
            Pid ! {self(), dict:fetch(Key, Dict)},
            loop(Dict);
        {has_key, Pid, Key} ->
            Pid ! {self(), dict:has_key(Key, Dict)},
            loop(Dict);
        {store, _, Key, Value} ->
            loop(dict:store(Key, Value, Dict));
        % Unsure of if this should send a message or not
        {stop, _} ->
            stop
    end.


fetch(Key, Pid) ->
    Pid ! {fetch, self(), Key},
    receive
        {Pid, Value} ->
            Value
    end.

has_key(Key, Pid) ->
    Pid ! {has_key, self(), Key},
    receive
        {Pid, Value} ->
            Value
    end.

store(Key, Value, Pid) ->
    Pid ! {store, Key, Value},
    ok.
