-module(client_SUITE).

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
    {ok, AppNames2} = application:ensure_all_started(chat_client),
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
    {ok, connecting} = chat_client:connect({127,0,0,1}, 8080 + 1, client_test1),
    {ok, connected} = recive_messages(),
    {ok, checking_name} = chat_client:signin("Name", "123", client_test1),
    {ok, logged} = recive_messages(),
    {ok, getting} = chat_client:get_rooms(client_test1),
    {rooms, _List} = recive_messages(),
    {ok, joining} = chat_client:join_room("Lobby", client_test1),
    {ok, joined} = recive_messages(),
    {ok, sending} = chat_client:send_message("Test -- 1", client_test1),
    {ok, sended} = recive_messages(),
    {new_message, _Message} = recive_messages(),
    {ok, disconnected} = chat_client:disconnect(client_test1),

    {ok, connecting} = chat_client:connect({127,0,0,1}, 8080 + 1, client_test1),
    {ok, connected} = recive_messages(),
    {ok, checking_name} = chat_client:signin("Name", "12", client_test1),
    {error, bad_password} = recive_messages(),
    {ok, checking_name} = chat_client:signin("Name", "123", client_test1),
    {ok, logged} = recive_messages(),

    % we in last joined room now
    {ok, loading} = chat_client:load_history(client_test1),
    {history, _Messages} = recive_messages(),

    {ok, connecting} = chat_client:connect({127,0,0,1}, 8080 + 1, client_test2),
    {ok, connected} = recive_messages(),
    {ok, checking_name} = chat_client:signin("Name2", "1234", client_test2),
    {ok, logged} = recive_messages(),
    {ok, getting} = chat_client:get_rooms(client_test2),
    {rooms, _List} = recive_messages(),
    {ok, joining} = chat_client:join_room("Lobby", client_test2),
    {ok, joined} = recive_messages(),

    {ok, sending} = chat_client:send_message("Test -- 2", client_test2),
    {ok, sended} = recive_messages(),
    {new_message, Message_2} = recive_messages(),
    {new_message, Message_2} = recive_messages(),

    {ok, sending} = chat_client:send_message("Test -- 3", client_test1),
    {ok, sended} = recive_messages(),
    {new_message, Message_3} = recive_messages(),
    {new_message, Message_3} = recive_messages(),

    {ok, quiting} = chat_client:quit_room("Lobby", client_test1),
    {ok, quited} = recive_messages(),

    {ok, sending} = chat_client:send_message("Test -- 4", client_test2),
    {ok, sended} = recive_messages(),
    {new_message, _Message_4} = recive_messages(),
    {error, timeout} = recive_messages(),

    {ok, disconnected} = chat_client:disconnect(client_test1),
    {ok, disconnected} = chat_client:disconnect(client_test2),
    ok.





