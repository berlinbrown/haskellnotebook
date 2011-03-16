%%----------------------------------------------------------
%% File:   server_lib
%% Descr:  IRC Server library gen_server
%% Author: Berlin Brown
%% Date:   3/2/2008
%%
%% Related Quotes:
%% --------------
%% Baron Harkonnen: I will have Arrakis back for myself! 
%% He who controls the Spice controls the universe and what 
%% Piter did not tell you is we have control of someone 
%% who is very close, very close, to Duke Leto! 
%% This person, this traitor, will be worth more to 
%% us than ten legions of Sardaukar!
%% --------------
%%
%% Additional Resources:
%% http://www.erlang.org/doc/man/gen_tcp.html
%% gen_tcp:listen:
%%   Received Packet is delivered as a binary.
%%       {ok, LSock} = gen_tcp:listen(5678, [binary, {packet, 0}, 
%%                                        {active, false}]),
%%----------------------------------------------------------

-module(server_lib).
-include("irc_server.hrl").
-behaviour(gen_server).

-export([start_link/1, get_cur_state/1, server_listen/1, server_accept_call/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, code_change/3,
         terminate/2]).

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
	io:format("trace: server_lib:init~n"),
	%{ok, Ref} = timer:send_interval(connection_timeout(), server_timeout),
	AppHandler = Client#irc_server_info.app_handler,
	{ok, #server_state{app_handler=AppHandler,
					   connection_timeout=undefined,
					   client=Client,
					   state=starting}}.

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
handle_call(irc_server_bind, _From,
			#server_state{client=Client } = State) ->
	Port = Client#irc_server_info.port,
	io:format("trace: lib:handle_call:bind Port:<~p>~n", [Port]),
    {ok, ServSock} = server_bind(Port),
    {reply, ok, State#server_state{serv_sock=ServSock, state=connecting}};
handle_call(irc_accept_clients, _From,
			#server_state{serv_sock=ServSock, app_handler=AppHandler } = State) ->

	io:format("trace: lib:handle_call accept_clients. [~p]~n", [AppHandler]),
	ClientSock = server_accept(ServSock),
	%***************
	% Start the handler process
	%***************
	io:format("trace: [!!] lib:handle_call client-socket: [~p]~n", [ClientSock]),
	% Pass the main app handler, serv lib handler, client sock
	% And launch the handler process.
	ClientInfo = #client_info{app_handler=AppHandler,serv_lib=self(),
							  client_sock=ClientSock},
	{ ok, ClientServ } = client_handler:start_link(ClientInfo),
	% Assign a new controlling process.
	gen_tcp:controlling_process(ClientSock, ClientServ),
	% Active the socket so that the client handler can handle the
	% tcp data.
	inet:setopts(ClientSock, [{packet, 0}, binary, 
							  {nodelay, true},{active, true}]),
	{reply, ok, State};
handle_call(get_cur_state, _From, #server_state{} = State) ->
	% Generic method to get the current state.
	io:format("trace: lib:handle_call:get_cur_state~n"),
	{reply, {ok, State}, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #server_state{client=Client}) ->
    io:format("trace: lib:terminate reason:[~p]~n", [_Reason]),
    ok;
terminate(normal, #server_state{client=Client}) ->
    io:format("trace: lib:terminate.normal~n"),
    ok;
terminate(shutdown, #server_state{client=Client}) ->
    io:format("trace: lib:terminate.shutdown~n"),
    ok.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(server_timeout, #server_state{client=Client} = State) ->
	io:format("trace: lib:handle_info.server_timeout"),
    %{noreply, State#server_state{state=timeout, connection_timeout=undefined}}.
	{noreply, State}.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Use the gen_server call to get ready to listen on port.
%% Parm:Client - irc_server_info
%%--------------------------------------------------------------------
server_listen(ServLib) ->  
	io:format("trace: lib:server listen [~p]~n", [ServLib]),
	% Synchronous gen_server call
	gen_server:call(ServLib, irc_server_bind).

%% Accept call and then cast to handle new client
server_accept_call(ServLib) ->
	io:format("trace: lib:server accept new client~n"),
	gen_server:call(ServLib, irc_accept_clients, infinity).

get_cur_state(ServLib) ->
	io:format("trace: lib:get_cur_state: pid:[~p] ~n", [ServLib]),
	% Return: {ok, State}
	gen_server:call(ServLib, get_cur_state).

%% gen_tcp IO call - server_bind(Port) -> { ok, LSock }
server_bind(Port) ->
	io:format("trace: attempting to bind server... [~p]~n", [Port]),
	gen_tcp:listen(Port, [binary, {packet, 0}, 
						  {active, false}]).

%%--------------------------------------------------------------------
%% Function: server_accept(ServSock) -> ClientSock
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
server_accept(ServSock) ->	
	{ ok, ClientSock } = gen_tcp:accept(ServSock),
	ClientSock.

connection_timeout() ->
    % Time out delay of 1 minute
    600000.

%% End of File
