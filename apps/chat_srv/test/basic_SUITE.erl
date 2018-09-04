-module(basic_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([basic_tests/1]).

all() -> [basic_tests].

init_per_testcase(basic_tests, Config) ->
    Config.

end_per_testcase(basic_tests, _Config) ->
    ok.

basic_tests(_Config) ->
    {ok, _Pid} = chat_srv_sup:start_link(),
    ok.
