-module(basic_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([basic_tests/1]).

all() -> [basic_tests].

init_per_testcase(basic_tests, Config) ->
    Config.

end_per_testcase(basic_tests, _Config) ->
    ok.

tcp_send(Socket, Tuple) ->
    Binary = erlang:term_to_binary(Tuple),
    logger:alert("send packet - ~p binary - ~p", [Tuple, Binary]),
    gen_tcp:send(Socket, Binary).

tcp_recv(Socket) ->
    {ok, Packet} = gen_tcp:recv(Socket, 0),
    Tuple = erlang:binary_to_term(Packet),
    logger:alert("receive packet - ~p which mean - ~p", [Packet, Tuple]),
    Tuple.

basic_tests(_Config) ->
    {ok, _Pid} = chat_srv_sup:start_link(),
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, 8080 + 1, [binary, {active, false}]),
    {ok, login} = tcp_recv(Socket),
    ok = tcp_send(Socket, {enter_name, "Name"}),
    {ok, user_not_found} = tcp_recv(Socket), % you are new user
    ok = tcp_send(Socket, {enter_new_password, "123"}),
    {ok, added_and_logged} = tcp_recv(Socket), % login
    ok = tcp_send(Socket, {disconnect, "dummy"}),
    {error,closed} = gen_tcp:recv(Socket, 0), % disconnect

    %% new connection
    {ok, Socket2} = gen_tcp:connect({127,0,0,1}, 8080 + 1, [binary, {active, false}]),
    {ok, login} = tcp_recv(Socket2),
    ok = tcp_send(Socket2, {enter_name, "Name"}),
    {ok, found} = tcp_recv(Socket2), % you are old user
    ok = tcp_send(Socket2, {enter_old_password, "12"}),
    {ok, bad_password} = tcp_recv(Socket2), % wrong password
    ok = tcp_send(Socket2, {enter_old_password, "123"}),
    {ok, logged} = tcp_recv(Socket2), % login
    ok = tcp_send(Socket2, {get_rooms, "dummy"}),
    {ok, ["Lobby"]} = tcp_recv(Socket2), % empty list
    ok.





