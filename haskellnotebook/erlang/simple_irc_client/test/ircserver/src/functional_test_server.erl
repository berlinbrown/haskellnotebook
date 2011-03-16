%%----------------------------------------------------------
%% File:   server_lib
%% Descr:  IRC Server library gen_server
%% Author: Berlin Brown
%% Date:   3/2/2008
%% <Currently deprecated>
%%----------------------------------------------------------

-module(functional_test_server).

-include("irc_server.hrl").

-export([start_test_serv/0, start/1, server_handler/1]).

start(Client) ->
	% Ref: spawn_link(Module, Function, ArgList)
    spawn_link(?MODULE, server_handler, [Client]).

%%
%% Server handler info:
%% lib_handler - gen_server process id created from invoking server_lib start link.
%%               In the function server_handler, this variable is defined as <P>.
%%               Use lib_handler to pass messages to the server library.
-record(serv_handler_info, {lib_handler}).

%%----------------------------------------------------------
%% Server Handler Operations:
%%
%% The server handler and the app handler 'wait_for_messages'
%% work together to handle incoming client requests.
%%----------------------------------------------------------
server_handler(Client) ->
	ServStart = server_lib:start_link(Client),
	case ServStart of
		{ ok, P } ->
			io:format("trace: app:server pid/lib [~p]~n", [P]),
			server_lib:server_listen(P),
			State = server_lib:get_cur_state(P),
			% io:format("trace: app:server state [~p]~n", [State]),
			server_handler(#serv_handler_info{lib_handler=P}, idle)
	end.

server_handler(ServClient, idle) ->
	ServLib = ServClient#serv_handler_info.lib_handler,
	io:format("trace: [!] at server_handler.idle [~p]~n", [ServLib]),
	receive
		{ ready_accept } ->
		    % Application process is ready to accept new clients			
			io:format("trace: app: wait_messages:accept <incoming>~n"),
			% Launch initial accept clients block
			AcceptCall = server_lib:server_accept_call(ServLib),
			io:format("!!!-->~p~n", [AcceptCall]),
			case AcceptCall of
				{ ok, State } ->
					io:format("Test~n");
				Else ->
					io:format("trace: server_handler.accept<else> [~p]~n", [Else])			
			end,
			server_handler(ServClient, idle);
		Error ->
			io:format("trace: [!] app:error.proceeed<idle>: ~p~n", [Error]),
			server_handler(ServClient, idle)
    after connection_timeout() ->
            io:format("trace: [!] at server_handler. TIMEOUT ~n"),
            server_handler(ServClient, idle)
    end.

accept_clients(ServHandler) ->
	% Start waiting accept new client connections.
    ServHandler ! {ready_accept}.

init_accept_clients() ->
	% Start waiting accept new client connections.
    self() ! {init_accept}.

%%----------------------------------------------------------
%% Application Entry Point
%%----------------------------------------------------------
start_test_serv() ->
	io:format("Running functional test~n"),
	% Ensure that the app_handler is associcated with this app process
	Client = #irc_server_info{app_handler=self()},
	Pid = start(Client),
	io:format("trace: <after start> start server, handler:~p ~n", [Pid]),
	% Wait a couple of seconds and then send messages to wait_for_clients.
	timer:sleep(500),
	init_accept_clients(),
	% Pass the pid of the server handler process.
	wait_for_clients(Pid),
	io:format("Done~n").

%%
%% Parm:Client - ServState
wait_for_clients(ServHandler) ->
	io:format("trace: app: wait_for_clients~n"),
    receive		
        Anything ->
			% Application process is ready to accept new clients
            io:format("trace: app: wait_messages:accept <incoming>~n"),
			accept_clients(ServHandler),
            wait_for_clients(ServHandler)
    after connection_timeout() + 20000 ->
            io:format("INFO: Timed out~n")
    end.

connection_timeout() ->
    % 1 minute
    600000.

%% End of File
