%%
%% File:   laughingman.erl
%% Date:   2/26/2008
%%
%%
-module(simple_irc_lib).

-export([start/3, stop/1, init/1]).
-export([message/2]).
-export([test/0]).

-include("irc.hrl").

-record(self, {handler, type, mode, args, sock}).

start(Type, Mode, Args) ->
    % Type is the atom chat or file
    % Mode is the atom client or server
    % Args are extra arguments depending on these
    spawn_link(?MODULE, init, [#self{handler=self(),
                                    type=Type,
                                    mode=Mode,
                                    args=Args}]).


init(#self{type=chat, mode=client, args=[Host, Port]} = Self) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [list, {active, once}]),
    loop(Self#self{sock=Sock}).

loop(#self{sock=Sock} = Self) ->
    inet:setopts(Sock, [{active, once}]),
    receive
        {tcp, Sock, Data} ->
            Self#self.handler ! {dcc_chat, self(), Data},
            loop(Self);
        {tcp_closed, Sock} ->
            Self#self.handler ! {dcc_closed, self()};
        {tcp_error, Sock, Reason} ->
            Self#self.handler ! {dcc_error, self(), Reason};

        {dcc_message, Message} ->
            ok = gen_tcp:send(Self#self.sock, Message),
            loop(Self);
        {stop, Pid} ->
            Pid ! stop
    end.


message(Client, Message) ->
    Client ! {dcc_message, Message}.

stop(Client) ->
    Client ! {stop, self()},
    receive
        stop ->
            stop
    end.

% Parse out the data from the dcc string
parse_dcc([1 | Rest]) ->
    parse_dcc(string:tokens(Rest, " "));
parse_dcc(["DCC", "CHAT", "CHAT", Host, Port]) ->
    {ok, chat, convert_from_numeric_ip(Host), element(1, string:to_integer(strip_terminater(Port)))}.

strip_terminater([1 | Val]) ->
    lists:reverse(Val);
strip_terminater(Val) ->
    strip_terminater(lists:reverse(Val)).

convert_from_numeric_ip(Host) ->
    list_to_tuple(binary_to_list(<<(element(1, string:to_integer(Host))):32>>)).

test() ->
    P = irc_lib:start_link(#irc_client_info{realname="laughingman24qwe", 
											nick="laughingman24qwe", 
											handler=self(), 
											servers=[{"irc.freenode.org", 6667}]}),
	case P of
		{ ok, Irclib } ->
			io:format("irc_lib:start_link ->~p ~n", [P]),
			timer:sleep(18000),
			irc_lib:join(Irclib, "#botlist"),
			timer:sleep(4000),
			irc_lib:msg(Irclib, "#botlist", "Hello"),
			timer:sleep(6000),
			irc_lib:msg(Irclib, "#botlist", "Hello"),
			timer:sleep(6000),
			irc_lib:quit(Irclib, "zonks"),
			irc_lib:stop(Irclib);
		{ error, _ } -> 
			io:format ("ERROR~n")	
	end.

get_privmsg(Client) ->
    receive
        {irc_message, Client, {_, "PRIVMSG", Args}} ->
            io:format("~w~n", [Args]),
            Args
    end.

%% End of File
