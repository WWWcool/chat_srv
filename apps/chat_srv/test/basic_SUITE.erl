-module(basic_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([basic_tests/1]).

%% tests descriptions

-type config() :: [{atom(), term()}].

-define(config(Key, C), (element(2, lists:keyfind(Key, 1, C)))).

-spec all() -> [atom()].
all() ->
    [
        basic_tests
    ].

%% starting/stopping
-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    {ok, _} = application:ensure_all_started(chat_srv),
    {ok, _} = application:ensure_all_started(chat_client),
    C.

-spec end_per_suite(config()) -> term().
end_per_suite(_C) ->
    ok.

%% tests
-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(basic_tests, C) ->
    C.

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(basic_tests, _C) ->
    ok.

tcp_send(Socket, Tuple) ->
    Binary = erlang:term_to_binary(Tuple),
    logger:alert("send packet - ~p binary - ~p", [Tuple, Binary]),
    gen_tcp:send(Socket, Binary).

tcp_recv(Socket) ->
    {ok, Packet} = gen_tcp:recv(Socket, 0, 1000),
    Tuple = erlang:binary_to_term(Packet),
    logger:alert("receive packet - ~p which mean - ~p", [Packet, Tuple]),
    Tuple.

new_client(Name, Password) ->
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, 8080 + 1, [binary, {active, false}]),
    {ok, login} = tcp_recv(Socket),
    ok = tcp_send(Socket, {enter_name, Name}),
    {ok, user_not_found} = tcp_recv(Socket), % you are new user
    ok = tcp_send(Socket, {enter_new_password, Password}),
    {ok, added_and_logged} = tcp_recv(Socket),
    {ok, Socket}. % login

basic_tests(_Config) ->
    {ok, Socket} = new_client("Name", "123"), % new client
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
    {ok, ["Lobby", "Room1"]} = tcp_recv(Socket2), % room list on server

    ok = tcp_send(Socket2, {send_message, "Test"}),
    {ok, room_not_found} = tcp_recv(Socket2), % we out of room now
    ok = tcp_send(Socket2, {join_room, "Lobby"}),
    {ok, joined} = tcp_recv(Socket2),
    %ok = tcp_send(Socket2, {join_room, "Lobby2"}),
    %{ok, room_not_found} = tcp_recv(Socket2), % wrong room name
    ok = tcp_send(Socket2, {load_history, "dummy"}),
    {ok, []} = tcp_recv(Socket2), % empty message list
    ok = tcp_send(Socket2, {send_message, "Test"}),
    {ok, start_resend} = tcp_recv(Socket2), % we in the room now
    {new_message, Message} = tcp_recv(Socket2),
    ok = tcp_send(Socket2, {load_history, "dummy"}),
    {ok, [Message]} = tcp_recv(Socket2),
    ok = tcp_send(Socket2, {change_room, "Room1"}),
    {ok, changed} = tcp_recv(Socket2),

    %% add one more client
    {ok, Socket3} = new_client("Second", "1234"), % new client
    ok = tcp_send(Socket3, {join_room, "Room1"}),
    {ok, joined} = tcp_recv(Socket3),

    ok = tcp_send(Socket2, {send_message, "Test -- 2"}),
    {ok, start_resend} = tcp_recv(Socket2),
    {new_message, Message2} = tcp_recv(Socket2),
    {new_message, Message2} = tcp_recv(Socket3),

    ok = tcp_send(Socket3, {send_message, "Test -- 3"}),
    {ok, start_resend} = tcp_recv(Socket3),
    {new_message, Message3} = tcp_recv(Socket3),
    {new_message, Message3} = tcp_recv(Socket2),

    ok = tcp_send(Socket3, {quit_room, "Room1"}),
    {ok, quited} = tcp_recv(Socket3),

    ok = tcp_send(Socket2, {send_message, "Test -- 4"}),
    {ok, start_resend} = tcp_recv(Socket2),
    {new_message, _Message4} = tcp_recv(Socket2),
    {error, timeout} = gen_tcp:recv(Socket3, 0, 1000),

    ok = gen_tcp:close(Socket2),
    ok = gen_tcp:close(Socket3),
    ok.





