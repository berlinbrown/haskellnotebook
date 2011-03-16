%%%-------------------------------------------------------------------
%%% File    : irc_bot2.erl
%%% Author  : ortitz <orbitz@blong.orbitz>
%%% Description : Irc bot gen_server
%%%
%%% Created : 23 Mar 2006 by ortitz <orbitz@blong.orbitz>
%%%-------------------------------------------------------------------
-module(irc_bot).

-include("db.hrl").
-include("irc.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API exports
-export([add_bot/1, say/3, stop/2]).
-export([get_irclib/1, get_nick/1, get_cur_state/1]).

-record(state, {nick, dict, state, irclib, pong_timeout=undefined, 
				connection_timeout, app_handler=undefined}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Client) ->
    gen_server:start_link(?MODULE, [Client], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%%
%% Description: Initiates the server
%% Whenever a gen_server is started using gen_server:start/3,4 
%% or gen_server:start_link/3,4, this function is called by the new process to initialize. 
%%--------------------------------------------------------------------
init([Client]) ->
	io:format("irc_bot:init~n"),
    {ok, Ref} = timer:send_interval(connection_timeout(), timed_ping),
    %ok = bot_manager:store(Client#irc_bot.botname, self()),
    {ok, Pid} = irc_lib:start_link(
                  #irc_client_info{nick=Client#irc_bot.nick,
                                   realname=Client#irc_bot.realname,
                                   servers=Client#irc_bot.servers,
                                   handler=self(),
                                   password=Client#irc_bot.password}),
	AppHandler = Client#irc_bot.handler,
	io:format("irc_bot:init start_link.pid: ~p~n", [Pid]),
    {ok, #state{irclib=Pid,
				app_handler=AppHandler,
                dict=dict_proc:start(dict:from_list([{join_on_connect, Client#irc_bot.channels}])),
                connection_timeout=Ref,				
                state=connecting}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} 
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(get_nick, _From, #state{nick=Nick} = State) ->
    {reply, {ok, Nick}, State};
handle_call(get_irclib, _From, #state{irclib=Irclib} = State) ->
    {reply, {ok, Irclib}, State};
handle_call({say, Where, What}, _From, #state{irclib=Irclib} = State) ->
	io:format("trace: irc_bot:handle_call:say()~n"),
    irc_lib:say(Irclib, Where, What),
    {reply, ok, State};
handle_call(get_cur_state, _From, #state{} = State) ->
	% Generic method to get the current state.
	io:format("trace: irc_bot:handle_call:get_cur_state~n"),
	{reply, {ok, State}, State};
handle_call({stop, Message}, _From, #state{irclib=Irclib} = State) ->
    irc_lib:quit(Irclib, Message),
    irc_lib:stop(Irclib),
    {stop, stop, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({irc_connect, Nick}, #state{state=connecting, dict=Dict, 
										irclib=Irclib} = State) ->
    % Do connect stuff
	io:format("trace: irc_bot:irc_connect~n"),
    join_channels(Irclib, dict_proc:fetch(join_on_connect, Dict)),
    {noreply, State#state{nick=Nick, state=idle}};
handle_cast({stop, _}, #state{state=connecting} = State) ->
    {stop, stop, State};
handle_cast({irc_message, {_, "PONG", _}}, #state{state=pong, pong_timeout=Ref} = State) ->
    {ok, cancel} = timer:cancel(Ref),
    {noreply, State#state{state=idle, pong_timeout=undefined}};
handle_cast(irc_closed, #state{irclib=Irclib} = State) ->
	io:format("trace: irc_bot:irc_closed~n"),
    irc_lib:connect(Irclib),
    {noreply, State#state{state=connecting}};
handle_cast({irc_message, {_, "PING", [Server]}}, #state{irclib=Irclib} = State) ->
    irc_lib:pong(Irclib, Server),
    {noreply, State};
handle_cast({irc_message, {_, "KICK", [Channel, Nick, _]}}, #state{irclib=Irclib, nick=Nick} = State) ->
    irc_lib:join(Irclib, Channel),
    {noreply, State};
handle_cast({irc_message, {_, "KICK", [Channel, Nick]}}, #state{irclib=Irclib, nick=Nick} = State) ->
    irc_lib:join(Irclib, Channel),
    {noreply, State};
handle_cast({irc_message, {_Who, "JOIN", [_Where]}}, State) ->
%%    flood_policy:handle({Who, Where, self()}, join, self()),
    {noreply, State};
handle_cast({irc_message, {From, "PRIVMSG", [To, Message]}}, 
			#state{ irclib=Irclib, app_handler=AppHandler } = State) ->
	%***********************************
    % Let's dispatch the message:
    % For message dispatching we always send the pid of the bot it originated from
    % in the message.  I havn't decided what other information will be helpful for this.
    % From the bot pid, they can determine the name of the bot using bot_manager
	%***********************************
	io:format("INFO: incoming PRIVMSG: from:[~p] channel:[~p]~n", 
			  [From, To]),
	AppHandler ! {irc_message, self(), {From, "PRIVMSG", [To, Message]}},
    msg_dispatch:dispatch("PRIVMSG", [From, To, Message], [self()]),
    {noreply, State};
handle_cast({say, Where, What}, #state{irclib=Irclib} = State) ->
    irc_lib:say(Irclib, Where, What),
    {noreply, State};
% Catch all
handle_cast({irc_message, _}, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(timedout_ping, #state{irclib=Irclib, state=pong} = State) ->
    irc_lib:disconnect(Irclib),
    irc_lib:connect(Irclib),
    {noreply, State#state{state=connecting}};
handle_info(timed_ping, #state{irclib=Irclib} = State) ->
    irc_lib:ping(Irclib),
    {ok, Ref} = timer:send_after(pong_timeout(), timedout_ping),
    {noreply, State#state{state=pong, pong_timeout=Ref}}.


%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{connection_timeout=Ref, dict=Dict}) ->
    timer:cancel(Ref),
    dict_proc:stop(Dict),
    %bot_manager:erase(self()),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

join_channels(Bot, [Channel | Rest]) ->
	io:format("trace: irc_bot:join_channels()"),  
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

% -------------------------------------------------------------
% Functions used to interact with the bot
say(Bot, Where, What) ->
	io:format("trace: irc_bot:say<attempt cast>~n"),
    gen_server:cast(Bot, {say, Where, What}).

stop(Bot, Message) ->
    gen_server:call(Bot, {stop, Message}).

get_irclib(Name) ->
    {ok, Pid} = bot_manager:fetch_pid(Name),
    gen_server:call(Pid, get_irclib).

get_nick(Bot) when is_pid(Bot) ->
    {ok, Nick} = gen_server:call(Bot, get_nick),
    Nick.

get_cur_state(Bot) ->
	{ok, State} = gen_server:call(Bot, get_cur_state),
	io:format("trace: irc_bot:get_cur_state: [~p]~n", [State]),
	{ok, State}.

% -------------------------------------------------------------
% Functions for manipulating the bot database
add_bot({Botname, Nick, Realname, Servers, Channels, Password}) ->
    p1_db:insert_row(#irc_bot_db{botname=Botname, nick=Nick, realname=Realname, servers=Servers, channels=Channels, password=Password}).

%% End of the file
