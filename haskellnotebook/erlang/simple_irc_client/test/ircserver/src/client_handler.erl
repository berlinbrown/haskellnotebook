%%----------------------------------------------------------
%% File:   server_lib
%% Descr:  IRC Server library gen_server
%% Author: Berlin Brown
%% Date:   3/2/2008
%%
%% Additional Resources:
%% http://www.erlang.org/doc/man/gen_tcp.html
%%----------------------------------------------------------

-module(client_handler).
-include("irc_server.hrl").
-behaviour(gen_server).

-export([start_link/1, get_cur_state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, code_change/3,
         terminate/2]).

-import_all(data_lib).

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
	AppHandler = Client#client_info.app_handler,
	ClientSock = Client#client_info.client_sock,
	io:format("trace: client_handler:init. handler:[~p]~n", [AppHandler]),
	{ok, #client_state{app_handler=AppHandler,
					   connection_timeout=undefined,
					   client_sock=ClientSock,
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
handle_call(get_cur_state, _From, #client_state{} = State) ->
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
terminate(_Reason, #client_state{client=Client}) ->
    io:format("trace: client:handler:terminate reason:[~p]~n", [_Reason]),
    ok;
terminate(normal, #client_state{client=Client}) ->
    io:format("trace: client:handler:terminate.normal~n"),
    ok;
terminate(shutdown, #client_state{client=Client}) ->
    io:format("trace: client:handler:terminate.shutdown~n"),
    ok;
terminate(noreply, #client_state{client=Client}) ->
    io:format("trace: client:handler:terminate.<noreply>~n"),
    ok;
terminate(_, State) ->
	io:format("trace: client:handler:terminate.<generic> [~p]~n", [State]),	
	ok.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, Sock, Data}, State) ->	
    inet:setopts(Sock, [{active, once}]),
	io:format("trace: lib:info.tcp data [~p]~n", [Data]),
 	{Prefix, Command, Args} =
 		try data_lib:scan_string(Data)
 		catch
 			_:X ->
 				io:format("ERROR: error attempting to parse input command error:[~p]~n",[X]),
 				{"", (Data), ""}
 		end,	
 	% Invoke the client data handler to process incoming messages.
 	HandleState=client_handle_data(State, {Prefix, Command, Args}),
    {noreply, State};
handle_info({tcp_closed, Sock}, State) ->
	%*************************
	% Client has closed the connection.
	%*************************
	io:format("trace: lib:info.tcp_closed state:[~p]~n", [State]),
	AppHandler = State#client_state.app_handler,
	inet:setopts(Sock, [{active, once}]),
	gen_tcp:close(Sock),
	% Send a request to the handler; we lost a connection.
	AppHandler ! {connection_closed},
    {noreply, State#client_state{state=disconn, client_sock=nil}};
handle_info({tcp_error, Sock, Reason}, State) ->
	io:format("trace: lib:info.tcp_error~n"),
    inet:setopts(Sock, [{active, once}]),
    {noreply, State#client_state{state=disconn}};
handle_info(Msg, State) ->
	% Generic handle info handler
	io:format("trace: lib:info.<generic> [~p] [~p]~n", [Msg,State]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% Internal functions
%% Including client handle data.
%%--------------------------------------------------------------------
client_handle_data(#client_state{app_handler=AppHandler, client=Client, client_sock=Sock},
								 {_, <<"sysquit\r\n">>, _}) ->
	io:format("trace: client_lib: data: system QUIT, sock:[~p] ~n", [Sock]),
	gen_tcp:send(Sock, "Shutting Down Server\r\n"),
	% Send a request to the handler; we lost a connection.
	AppHandler ! {shutdown},
    idle;
client_handle_data(#client_state{app_handler=AppHandler, client=Client, client_sock=Sock},
								 {_, <<"syshello\r\n">>, _}) ->
	gen_tcp:send(Sock, "Hello\r\n"),
    idle;
client_handle_data(#client_state{app_handler=AppHandler, client=Client}, Message) ->
    % Generic handler function.
	io:format("trace: client_lib: msg:[~p]~n", [Message]),
    idle.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_cur_state(ServLib) ->
	io:format("trace: lib:get_cur_state: pid:[~p] ~n", [ServLib]),
	% Return: {ok, State}
	gen_server:call(ServLib, get_cur_state).

%% End of File
