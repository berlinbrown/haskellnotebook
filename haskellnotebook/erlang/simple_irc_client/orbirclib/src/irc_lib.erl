%%%-------------------------------------------------------------------
%%% File    : irc_lib2.erl
%%% Author  : ortitz <orbitz@blong.orbitz>
%%% Description : 
%%%
%%% Created :  9 Mar 2006 by ortitz <orbitz@blong.orbitz>
%%%
%%% Revision History:
%%%
%%% + 2/28/2008 - Berlin Brown:
%%% The handler in handle_class must be an irc_bot gen_server
%%% Handler = handle_cast:irc_message, irc_closed
%%%
%%% References:
%%% http://erlang.org/doc/man/gen_server.html
%%%-------------------------------------------------------------------
-module(irc_lib).
-include("irc.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API Functions
-export([whois/2, join/2, stop/1, quit/1, quit/2, say/3, msg/3, pong/2, connect/1, disconnect/1, ping/1]).
-export([decode_mask/1]).

-export([split_once/2]).

-record(state, {sock, handler, client, state=idle}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Client) ->
    irc_lookup:start(),
    gen_server:start_link(?MODULE, [Client], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Client]) ->
    Dict = dict_proc:start(dict:from_list([
                                           {nick, Client#irc_client_info.nick},
                                           {realname, Client#irc_client_info.realname},
                                           {servers, Client#irc_client_info.servers},
                                           {password, Client#irc_client_info.password},
                                           {handler, Client#irc_client_info.handler}])),
    {ok, Sock} = connect_to_next_server(Dict),
    {ok, #state{sock=Sock, handler=dict_proc:fetch(handler, Dict), client=Dict, state=connecting}}.

%%--------------------------------------------------------------------
%% From = is a tuple {Pid, Tag} where Pid is the 
%%        pid of the client and Tag is a unique tag. 
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(irc_stop, _From, #state{state=stop} = State) ->
	io:format("trace: handle_call:stop~n"),
    {stop, success, irc_stop, State#state{state=stop}};
handle_call(irc_stop, _From, #state{state=disconn} = State) ->
	io:format("trace: handle_call:stop~n"),
    {stop, success, irc_stop, State#state{state=stop}};
handle_call(irc_connect, _From, #state{client=Client, state=disconn} = State) ->
    % Need to fix this so it connects and adjusts state accordingly
	io:format("trace: handle_call:irc_connect~n"),
    {ok, Sock} = connect_to_next_server(Client),
    {reply, ok, State#state{sock=Sock, state=connecting}};
handle_call(irc_disconnect, _From, #state{sock=Sock} = State) ->
    ok = gen_tcp:close(Sock),
    {reply, ok, State#state{sock=nil, state=disconn}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({irc_connect}, #state{client=Client, state=disconn} = State) ->
	io:format("trace: handle_cast:irc_connect~n"),
    {ok, Sock} = connect_to_next_server(Client),
    {noreply, State#state{sock=Sock, state=connecting}};
handle_cast({irc_stop}, #state{client=Client, state=disconn} = State) ->
	io:format("trace: handle_cast:irc_stop~n"),
    {noreply, State#state{state=stop}};
handle_cast({irc_send_command, {"PING"}}, #state{sock=Sock, client=Client} = State) ->
    send_command(Sock, {"PING", [dict_proc:fetch(nick, Client)]}),
    {noreply, State};
handle_cast({irc_send_command, {"NICK", [Nick]}}, #state{sock=Sock, client=Client} = State) ->
    send_command(Sock, {"NICK", [Nick]}),
    dict_proc:store(nick, Nick, Client),
    {noreply, State};
handle_cast({irc_send_command, Command}, #state{sock=Sock, handler=Handler} = State) ->
	io:format("trace: handle_cast [sock: ~p, cmd:~p]~n", [Sock, Command]),
    send_command(Sock, Command),	
	io:format("trace: irc_lib:irc_send_command:handle_cast <end> state:[~p] handler:[~p]~n----~n", [State, Handler]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, Sock, Data}, State) ->
    inet:setopts(Sock, [{active, once}]),
    {Prefix, Command, Args} = scan_string(Data),
    {noreply, State#state{state=handle_data(State, {Prefix, irc_lookup:translate_command(Command), Args})}};
handle_info({tcp_closed, Sock}, #state{handler=Handler} = State) ->
	io:format("trace: irc_lib:tcp_closed.handle_info [~p] [~p]~n", [Handler, State]),
    inet:setopts(Sock, [{active, once}]),
    gen_server:cast(Handler, irc_closed),
    {noreply, State#state{state=disconn}};
handle_info({tcp_error, Sock, Reason}, #state{handler=Handler} = State) ->
    inet:setopts(Sock, [{active, once}]),
    gen_server:cast(Handler, {irc_error, Reason}),
    {noreply, State#state{state=disconn}}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{client=Client}) ->
    dict_proc:stop(Client),
    ok;
terminate(shutdown, #state{client=Client}) ->
    dict_proc:stop(Client),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

% This function sends commands off to the handler, if required it will also build up certina message (just a tupple or list)
% For example a whois response needs to be built in roder to be useful, same with a join
% Handle a whois
handle_data(_, {_, "RPL_WHOISUSER", Args}) ->
    {whois, [{user, Args}]};
handle_data(#state{state={whois, Info}}, {_, "RPL_WHOISSERVER", Args}) ->
    {whois, [{server, Args} | Info]};
handle_data(#state{state={whois, Info}}, {_, "320", _}) ->
    % Some servers suppor this (like freenode)
    %{whois, [{identified, Args} | Info]};  % I dont' think we need teh ARgs stuff
    {whois, [identified | Info]};
handle_data(#state{state={whois, Info}}, {_, "RPL_WHOISIDLE", Args}) ->
    {whois, [{idle, Args} | Info]};
handle_data(#state{state={whois, Info}}, {_, "RPL_WHOISCHANNELS", Args}) ->
    {whois, [{channels, Args} | Info]};
handle_data(#state{state={whois, Info}}, {_, "RPL_WHOISOPERATOR", Args}) ->
    {whois, [{oper, Args} | Info]};
handle_data(#state{handler=Handler, state={whois, Info}}, {_, "RPL_ENDOFWHOIS", _}) ->
    gen_server:cast(Handler, {irc_message, whois, Info}),
    idle;
% Send login data when we connect
handle_data(#state{state=connecting, client=Client, sock=Sock}, _) ->
    case dict_proc:fetch(password, Client) of
        undefined ->
            ok;
        Pass ->
            send_command(Sock, [{"PASS", [Pass]}])
    end,
    send_command(Sock, [{"NICK", [dict_proc:fetch(nick, Client)]}, {"USER", ["1", "2", "3", ":" ++ dict_proc:fetch(realname, Client)]}]),
    nick_verify;
% Make sure the nick is good and if so tell the client they are connected
handle_data(#state{state=nick_verify, sock=Sock, client=Client}, {_, "ERR_NICKCOLLISION", _}) ->
    dict_proc:store(nick, dict_proc:fetch(nick, Client) ++ "_", Client),
    send_command(Sock, [{"NICK", [dict_proc:fetch(nick, Client)]}, {"USER", ["1", "2", "3", ":" ++ dict_proc:fetch(realname, Client)]}]),
    nick_verify;
handle_data(#state{state=nick_verify} = State, {_, "ERR_NICKNAMEINUSE", _}) ->
    handle_data(State, {'_', "ERR_NICKCOLLISION", '_'});
handle_data(#state{state=nick_verify, handler=Handler, client=Client} = State, {_, "RPL_WELCOME", _ }) ->
	io:format("trace: handle_data rpl_welcome:[~p][~p][~p]~n", [Handler, Client, nick_verify]),
	io:format("-----~ntrace: rpl_welcome state:[~p]~n", [State]),
	gen_server:cast(Handler, { irc_connect, dict_proc:fetch(nick, Client) }),
    idle;
handle_data(#state{state=nick_verify}, _) ->
    nick_verify;
% Anything else should get sent to the handler
handle_data(#state{handler=Handler, client=Client}, Message) ->
	%%********************************************
	%% Example messages at this point include (E.g. a message from a user)
	%% handle_data<else>: message:[{"blbrown!n=Berlin@ip",
    %%                                   "PRIVMSG",
    %%                                   ["#botlist",
    %%                                    "would you like some cake"]}]
	%%********************************************
	%io:format("|||trace: handle_data<else>: message:[~p]~nclient:[~p]~nhandler:[~p] [END]~n", [Message, Client, Handler]),
	%io:format("~p||| ", [Message]),
    gen_server:cast(Handler, {irc_message, Message}),
    idle.

%%--------------------------------------------------------------------
%% END OF HANDLE DATA
%%--------------------------------------------------------------------

% Parsing functions
split_once(String, Char) ->
    case string:chr(String, Char) of
        0 ->
            {"", String};
        Idx ->
            {string:substr(String, 1, Idx - 1), string:substr(String, Idx + 1)}
    end.

parse_args(String) ->
    parse_args(String, string:str(String, " :")).

parse_args(String, 0) ->
    string:tokens(String, " ");
parse_args(String, Idx) ->
    {S, Trail} = {string:substr(String, 1, Idx - 1), string:substr(String, Idx + 2)},
    Args = string:tokens(S, " "),
    Args ++ [Trail].

strip_lineend(Data) ->
    strip_lineend_(lists:reverse(Data)).

strip_lineend_([$\n | Data]) ->
    strip_lineend_(Data);
strip_lineend_([$\r | Data]) ->
    strip_lineend_(Data);
strip_lineend_(Data) ->
    lists:reverse(Data).

% Parse a line up
scan_string([$: | Data]) ->
    {Prefix, Temp} = split_once(Data, $\s),
    {_, Command,  Args} = scan_string(Temp),
    {Prefix, Command, Args};
scan_string(Data) ->
    [Command | Args] = parse_args(strip_lineend(Data)),
    {"", Command, Args}.
   
send_command(Sock, {Command, Params})  ->
    ok = gen_tcp:send(Sock, lists:foldl(
                              fun(Elm, Acc) ->
                                      Acc ++ " " ++ Elm
                              end, "", [Command | Params]) ++ "\r\n");
send_command(Sock, {Command}) ->
    ok = gen_tcp:send(Sock, Command ++ "\r\n");
send_command(Sock, [Command | Rest]) ->
    send_command(Sock, Command),
    send_command(Sock, Rest);
send_command(_, []) ->
    ok.

% This extracts the current server/port out of the client and appends it to the list and returns
% The new client and the server and port
get_next_server(Servers) ->
    [{Server, Port} | Tail] = Servers,
    % Eeck there must be a better way to do this
    {{Tail ++ [{Server, Port}]}, Server, Port}.

% Create a connection to an IRC server and return the socket
connect(Server, Port) ->
    gen_tcp:connect(Server, Port, [list, {active, once}, {packet, line}]).

connect_to_next_server(Dict) ->
	io:format("trace: connect_to_next_server ~p~n", [Dict]),
    {Serverlist, Server, Port} = get_next_server(dict_proc:fetch(servers, Dict)),
    dict_proc:store(servers, Serverlist, Dict),
    % This is horribly broken and should be done in handle_info or handle_call whatever
    connect(Server, Port).
    
%% ----------------------------------
%% Public interface
%% ---------------------------------
send_client_command(Irclib, Command, Args) ->
	io:format("trace: irc_lib.invoke cast(1) {{ ~p|~p ~p }}~n", [Irclib, Command, Args]),
    gen_server:cast(Irclib, {irc_send_command, {Command, Args}}).

send_client_command(Irclib, Command) ->
	io:format("trace: irc_lib.invoke cast(2)~p~n", [Irclib]),
    gen_server:cast(Irclib, {irc_send_command, {Command}}).

% Functions used to send various command sto the client
pong(Irclib, Server) ->
    send_client_command(Irclib, "PONG", [Server]).

ping(Irclib) ->
    send_client_command(Irclib, "PING").

whois(Irclib, Who) ->
    send_client_command(Irclib, "WHOIS", [Who]).

join(Irclib, {Channel, Pass}) ->
	io:format("trace: join<a>@~p~n", [Channel]),
    send_client_command(Irclib, "JOIN", [Channel, Pass]);
join(Irclib, Channel) when list(Channel) ->
	io:format("trace: join<b>@~p~n", [Channel]),
    send_client_command(Irclib, "JOIN", [Channel]).

quit(Irclib) ->
    send_client_command(Irclib, "QUIT").

quit(Irclib, Message) ->
    send_client_command(Irclib, "QUIT", [":" ++ Message]).
%%     receive
%%         {irc_closed, Irclib} ->
%%             ok
%%     end.

stop(Irclib) ->
	io:format("trace: irc_lib:stop: [~p]~n", [Irclib]),
	PidTag = {ok, Irclib},
	gen_server:call(Irclib, irc_stop).
%%     Irclib ! {irc_stop, self()},
%%     receive
%%         {irc_stop, Irclib} ->
%%             ok
%%     end.

say(Irclib, Where, What) ->
	io:format("trace: irc_lib.say~n"),
    send_client_command(Irclib, "PRIVMSG", [Where, ":" ++ What]).

msg(Irclib, Where, What) ->
	io:format("trace: irc_lib.msg~n"),
    send_client_command(Irclib, "PRIVMSG", [Where, ":" ++ What]).

connect(Irclib) ->
	io:format("trace: <attempt:call>connecting...~n"),
    gen_server:call(Irclib, irc_connect).
%%     Irclib ! {irc_connect, self()}.

disconnect(Irclib) ->
    gen_server:call(Irclib, irc_disconnect).
%%     Irclib ! {irc_disconnect, self()}.

% ----------------------------------------------------------------------------------------------------
% Parse out the various information from a mask
decode_mask(Mask) ->
    decode_mask(nick, Mask, {"", "", ""}).

decode_mask(nick, [$! | Rest], Info) ->
    decode_mask(ident, Rest, Info);
decode_mask(nick, [C | Rest], {Nick, Ident, Host}) ->
    decode_mask(nick, Rest, {[ C | Nick], Ident, Host});
decode_mask(nick, [], _) ->
    {error, bad_mask};
decode_mask(ident, [$@ | Rest], Info) ->
    decode_mask(host, Rest, Info);
decode_mask(ident, [C | Rest], {Nick, Ident, Host}) ->
    decode_mask(ident, Rest, {Nick, [C | Ident], Host});
decode_mask(ident, [], _) ->
    {error, bad_mask};
decode_mask(host, [C | Rest], {Nick, Ident, Host}) ->
    decode_mask(host, Rest, {Nick, Ident, [C | Host]});
decode_mask(host, [], {Nick, Ident, Host}) ->
    {ok, lists:reverse(Nick), lists:reverse(Ident), lists:reverse(Host)}.

%% End of File
