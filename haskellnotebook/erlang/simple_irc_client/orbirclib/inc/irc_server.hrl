%% irc_server.hrl
%%

%% Default IRC server port number is 6667
-record(irc_server_info, {port=9003, app_handler=undefined}).

%% server state info:
%% -----------------
%% serv_sock - server socket that is created once the server has bound to
%%             a particular port.
%% client - Client holds the port information and app_handler process id.
%%          The app handler pid is passed to this gen_server during the launch process call.
%%          E.g. app_handler value could be the application pid <0.1.0>
-record(server_state, {serv_sock, client, state, 
					   connection_timeout, app_handler=undefined}).

%% server state info:
%% -----------------
%% Note: (A little bit of redundancy) client info is passed to the client handler
%% when the process is started, the state information keeps the state during
%% client handler operations.
-record(client_info, { app_handler, serv_lib, client_sock }).
-record(client_state, { app_handler, serv_lib, state, client, client_sock, 
						connection_timeout=undefined }).

%% End of File.
