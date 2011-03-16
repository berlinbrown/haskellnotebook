%%
%% The laughingman appeared in public on February 3, 2024.
%% Author: Berlin Brown
%% Date:   2/26/2008
-module(laughingman).

-include("irc.hrl").

-export([start_laughingman/0, start/1, stop/2, proceed/1]).
-export([say/3]).

-record(self, {client, nick, configuration, handler, dict, info, irclib}).
%% For the 'state', see irc_bot
-record(state, {nick, dict, state, irclib, pong_timeout=undefined, 
				connection_timeout, app_handler=undefined}).

-define(BOT_NICK, "laughingman24").
-define(DEFAULT_CHAN, "#botlist").

start(Client) ->
	% Ref: spawn_link(Module, Function, ArgList)
    spawn_link(?MODULE, proceed, [Client]).

proceed(Client) ->
	Configuration = dict_proc:start(dict:from_list([{join_on_connect, 
													 Client#irc_bot.channels}])),
	StartLinkRes = irc_bot:start_link(Client),
	case StartLinkRes of 
		{ ok, Ircbot } ->
			io:format("trace: after:start_link=[~p] bothandler:[~p]~n", [StartLinkRes, Ircbot]),
 			IrcbotState = irc_bot:get_cur_state(Ircbot),
			io:format("trace: app:proceed.botstate:[~p]~n", [IrcbotState]),
 			case IrcbotState of
 				{ ok, State } ->
 					Irclib = State#state.irclib,
  					io:format("Ok: proc: irclib: [~p]~n", [Irclib]),
  				 	timer:sleep(10000),
					% Note: Handler, irc_bot.handler = self(), current pid.
 					Handler = Client#irc_bot.handler,
   					io:format("invoking proceed<for idle>...~n"),
   					proceed(#self{ client=Ircbot,
   								   nick=Client#irc_bot.nick,
   								   dict=Client#irc_bot.channels,
   								   configuration=Configuration,
 								   irclib=Irclib,
   								   info=Client,
   								   handler=self()}, idle)
 			end;
		ignore ->
			io:format("IGNORE~n");
		{ error, Error } ->
			io:format("ERROR: ~p~n", [Error])
	end.
proceed(Self, connecting) ->
    Ircbot  = Self#self.client,
    Handler = Self#self.handler,
	Nick    = Self#self.nick,
	Info    = Self#self.info,
	io:format("trace: proceed: IrcbotInfo: [~p] [handler:~p] ~n", [Ircbot, Handler]),
	io:format("---- IrcbotInfo END ----~n"),
    receive
        { irc_connect, Ircbot, Nick} ->
			io:format("trace: app:irc_connect~n"),
            % Do connect stuff
            join_channels(Ircbot, dict_proc:fetch(join_on_connect, 
												  Self#self.dict)),
            proceed(Self#self{nick=Nick}, idle);
        % Only the handler can stop it
        { stop, Handler, _ } ->
			io:format("trace: app:stop~n"), 
            Handler ! {stop, self()};
        Error ->
			io:format("trace: [!] app:error at proceeed: ~p~n", [Error]),
            %exit({'EXIT', Error})
			proceed(Self, idle)
    end,
	io:format("trace: app:process, waiting on proceed~n");
proceed(Self, pong) ->
    Client=Self#self.client,
    receive
        {irc_message, Client, {_, "PONG", _}} ->
            proceed(Self, idle)
    after pong_timeout() ->
            irc_lib:disconnect(Client),
            irc_lib:connect(Client),
            proceed(Self, connecting)
    end;
proceed(Self, idle) ->
	Ircbot  = Self#self.client,
    Handler = Self#self.handler,
	Nick    = Self#self.nick,
	Info    = Self#self.info,
	Irclib  = Self#self.irclib,
	io:format("trace: [!] at proceed.idle [~p][~p]~n", [Handler, Ircbot]),
    receive
        { irc_closed, Ircbot } ->
			io:format("trace: app:proceed.idle - irc_closed~n"),
            irc_lib:connect(Ircbot),
            proceed(Self, connecting);
		% Various messages we'll try to handle
        % Respond to a ping
        { irc_message, Ircbot, { _, "PING", [Server]} } ->
			io:format("trace: app:proceed.idle - message/ping~n"),
            irc_lib:pong(Ircbot, Server),
            proceed(Self, idle);
		{ irc_message, Ircbot, {From, "PRIVMSG", [To, Message]}} ->
			%***************************
			% Msssage handler, for PRIVMSG
			%***************************
			io:format("Message: ~p [~p]~n", [Message, To]),
			case (To == ?DEFAULT_CHAN) of
				true ->
					agent_lib:process_msg(Irclib, To, Message);
				false ->
					nothing
			end,
            proceed(Self, idle);
        % Catch all other IRC messages
        { irc_message, Ircbot, _ } ->
			io:format("trace: app:proceed.idle - message~n"),
            proceed(Self, idle);
        % Stuff from the client
        { say, Where, What} ->
			io:format("trace: app:proceed.idle - say handler:[~p] ~n", [Handler]),
            irc_lib:say(Irclib, Where, What),
            proceed(Self, idle);
        { msg, Where, What} ->
			io:format("trace: app:proceed.idle - msg handler:[~p] ~n", [Handler]),
            irc_lib:msg(Irclib, Where, What),
            proceed(Self, idle);
        { stop, Handler, Message} ->
			io:format("trace: app:proceed.idle - stop [~p]~n", [Handler]),
            irc_lib:quit(Irclib, Message),
            irc_lib:stop(Irclib),
			io:format("[.]~n"),
            Handler ! {stop, Handler};
		Error ->
			io:format("trace: [!] app:error.proceeed<idle>: ~p~n", [Error]),
			proceed(Self, idle)
    after connection_timeout() ->
            irc_lib:ping(Irclib),
            proceed(Self, pong)
    end.

join_channels(Bot, [Channel | Rest]) ->
    irc_lib:join(Bot, Channel),
    join_channels(Bot, Rest);
join_channels(_, []) ->
    ok.

connection_timeout() ->
    % 10 minutes
    600000.

pong_timeout() ->
    % 10 Seconds
    10000.

%% -------------------------------------------------------------
%% Functions used to interact with the bot
%% -------------------------------------------------------------
say(Bot, Where, What) ->
    Bot ! {say, Where, What}.

msg(Bot, Where, What) ->
    Bot ! {msg, Where, What}.

stop(Bot, Message) ->
	io:format("trace: app:stop() bot:~p msg:~p~n", [Bot, Message]),
    Bot ! {stop, Bot, Message},
    receive
        {stop, Bot} -> 
			io:format("trace: app:got stop~n"),
			ok
    end.

%% -------------------------------------------------------------
%% Start the laughing man
%% -------------------------------------------------------------
start_laughingman() ->
	irc_lookup:start(),
	% self() at this level is associated with the launching process
	% E.g. <0.1.0> 
	Client = #irc_bot{nick=?BOT_NICK, 
					  realname=?BOT_NICK,
					  handler=self(),
					  servers=[{"irc.freenode.net", 6667}],
					  channels=[?DEFAULT_CHAN]},
	P = start(Client),
 	io:format("trace: <after start> start_laughingman ->~p ~n", [P]),
 	timer:sleep(15000), 
 	msg(P, ?DEFAULT_CHAN, "Hello, I am the laughingman; would you like to have some fun."),
	timer:sleep(2000),
	wait_for_messages(P),
 	stop(P, "bye"),
	io:format("trace app:done.laughingman~n"),
	irc_lookup:shutdown().

wait_for_messages(Client) ->
    receive
        Anything ->
            io:format("trace: app: wait_messages<incoming>~n"),
			% Redirect the message to the client handler.
			% @see proceed
			Client ! Anything,
            wait_for_messages(Client)
    after connection_timeout() + 100000 ->
            io:format("INFO: Timed out~n")
    end.

%% End of File
