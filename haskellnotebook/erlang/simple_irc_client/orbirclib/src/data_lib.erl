-module(data_lib).

%% API
-export([split_once/2, parse_args/1, strip_lineend/1, scan_string/1]).

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

%% End of File
