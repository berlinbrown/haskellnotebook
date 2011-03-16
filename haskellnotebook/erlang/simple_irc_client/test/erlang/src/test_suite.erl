%%
%% Run all of the exported unit test cases.
%%

-module(test_suite).
-export([run_tests/0]).
-include_lib("eunit/include/eunit.hrl").

run_tests() ->
	eunit_examples:run_tests(),
	test_parse_data:run_tests(),
	test_simple_server:run_tests(),
	test_shutdown_server:run_tests().

%% End of File
