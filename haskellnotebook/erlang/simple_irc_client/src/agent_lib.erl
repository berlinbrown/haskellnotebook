%%
%% The laughingman appeared in public on February 3, 2024.
%% Author: Berlin Brown
%% Date:   2/26/2008
%%
%% Useful Resources:
%% http://erlang.org/doc/man/string.html
-module(agent_lib).
-include("irc.hrl").

-export([process_msg/3]).

-define(BOT_PREFIX, "laughingman").

process_msg(Irclib, Channel, Message) ->
	L = string:to_lower(Message),
	SS = lists:sublist(L, 1, 11),
	case (?BOT_PREFIX == SS) of
		true ->
			case regexp:first_match(Message, "help") of
				{ match, _, _ } ->
					irc_lib:msg(Irclib, Channel, "So, do you want help with something.  I am the wrong bot for the job.");
				nomatch ->
					ignore
			end,
			case regexp:first_match(Message, "foaf") of
				{ match, _, _ } ->
					irc_lib:msg(Irclib, Channel, "foaf is pretty cool, you can find out more here: http://www.foaf-project.org/.");
				nomatch ->
					ignore
			end,
			case regexp:first_match(Message, "http") of
				{ match, _, _ } ->
					irc_lib:msg(Irclib, Channel, "We are about to add you, give me a second.");
				nomatch ->
					ignore
			end;
		false ->
			nothing
	end.

%% End of File
