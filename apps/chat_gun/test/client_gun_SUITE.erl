-module(client_gun_SUITE).

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
    {ok, _AppNames} = application:ensure_all_started(chat_srv),
    {ok, AppNames2} = application:ensure_all_started(chat_gun),
    logger:alert("started apps - ~p", [AppNames2]),
    C.

-spec end_per_suite(config()) -> term().
end_per_suite(_C) ->
    ok.

%% tests
init_per_testcase(basic_tests, C) ->
    C.

end_per_testcase(basic_tests, _C) ->
    ok.

recive_messages() ->
    receive
        Any -> Any
    after
        1000 -> {error, timeout}
    end.

basic_tests(_Config) ->
    {ok, connecting} = gun_client:connect("localhost", 8080, client_gun1),
    {ok, connected} = recive_messages(),
    {ok, checking_name} = gun_client:signin("Gun", "123", client_gun1),
    {ok, logged} = recive_messages(),
    {ok, getting} = gun_client:get_rooms(client_gun1),
    {rooms, _List} = recive_messages(),
    {ok, joining} = gun_client:join_room(<<"Lobby">>, client_gun1),
    {ok, joined} = recive_messages(),
    {ok, sending} = gun_client:send_message("Test -- 1", client_gun1),
    {ok, sended} = recive_messages(),
    {new_message, _Message} = recive_messages(),
    {ok, disconnected} = gun_client:disconnect(client_gun1),

    {ok, connecting} = gun_client:connect("localhost", 8080, client_gun1),
    {ok, connected} = recive_messages(),
    {ok, checking_name} = gun_client:signin("Gun", "12", client_gun1),
    {error, bad_password} = recive_messages(),
    {ok, checking_name} = gun_client:signin("Gun", "123", client_gun1),
    {ok, logged} = recive_messages(),

    % we in last joined room now
    {ok, loading} = gun_client:load_history(client_gun1),
    {history, _Messages} = recive_messages(),

    {ok, connecting} = gun_client:connect("localhost", 8080, client_gun2),
    {ok, connected} = recive_messages(),
    {ok, checking_name} = gun_client:signin("Gun2", "1234", client_gun2),
    {ok, logged} = recive_messages(),
    {ok, getting} = gun_client:get_rooms(client_gun2),
    {rooms, _List} = recive_messages(),
    {ok, joining} = gun_client:join_room(<<"Lobby">>, client_gun2),
    {ok, joined} = recive_messages(),

    {ok, sending} = gun_client:send_message("Test -- 2", client_gun2),
    {ok, sended} = recive_messages(),
    {new_message, Message_2} = recive_messages(),
    {new_message, Message_2} = recive_messages(),

    {ok, sending} = gun_client:send_message("Test -- 3", client_gun1),
    {ok, sended} = recive_messages(),
    {new_message, Message_3} = recive_messages(),
    {new_message, Message_3} = recive_messages(),

    {ok, quiting} = gun_client:quit_room(<<"Lobby">>, client_gun1),
    {ok, quited} = recive_messages(),

    {ok, sending} = gun_client:send_message("Test -- 4", client_gun2),
    {ok, sended} = recive_messages(),
    {new_message, _Message_4} = recive_messages(),
    {error, timeout} = recive_messages(),

    {ok, disconnected} = gun_client:disconnect(client_gun1),
    {ok, disconnected} = gun_client:disconnect(client_gun2),
    ok.





