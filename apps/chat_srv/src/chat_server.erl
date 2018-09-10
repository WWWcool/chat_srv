-module (chat_server).

-export ([init_users/0]).
-export ([init_rooms/0]).
-export ([disconnect/2]).
-export ([check_name/2]).
-export ([login/4]).
-export ([new_user/4]).
-export ([get_rooms/1]).
-export ([join_room/3]).
-export ([quit_room/3]).
-export ([change_room/3]).
-export ([load_history/3]).
-export ([send_message/4]).
-export ([get_monitor/2]).

-type users() :: map().
-type rooms() :: map().
-type user() :: chat_user:user().

-export_type([users/0]).
-export_type([rooms/0]).

-spec init_users() -> users().

init_users() -> #{}.

-spec init_rooms() -> rooms().

init_rooms() ->
    LobbyName = <<"Lobby">>,
    Lobby = chat_room:add(LobbyName),
    RoomName = <<"Room1">>,
    Room = chat_room:add(RoomName),
    #{LobbyName => Lobby, RoomName => Room}.

-spec disconnect(pid() | iolist(), users()) -> {_, users()}.

disconnect(Pid, Users) when is_pid(Pid)->
    Name = maps:fold(
        fun(Name, SomeUser, NameList) ->
            case Pid =:= chat_user:get_pid(SomeUser) of
                true -> Name;
                false -> NameList
            end
        end, undefined, Users),
    disconnect(Name, Users);

disconnect(Name, Users) ->
    case maps:find(Name, Users) of
        {ok, User} ->
            NewUser = chat_user:logout(User),
            {{ok, disconnected}, maps:put(Name, NewUser, Users)};
        error ->
            {{ok, user_not_found}, Users}
    end.

-spec check_name(iolist(), users()) -> {ok, found | user_not_found}.

check_name(Name, Users) ->
    case maps:find(Name, Users) of
        {ok, _} ->
            {ok, found};
        error ->
            {ok, user_not_found}
    end.

-spec check_password(iolist(), iolist(), users()) -> {ok, user_not_found} | {_, user()}.

check_password(Name, Password, Users) ->
    case maps:find(Name, Users) of
        {ok, User} ->
            Reply = chat_user:check_password(Password, User),
            {Reply, User};
        error ->
            {ok, user_not_found}
    end.

-spec new_user(iolist(), iolist(), {pid(), reference()}, users()) -> {_, users()}.

new_user(Name, Password, {Pid, Monitor}, Users) ->
    User = chat_user:new(Name, Password, chat_user:new_connection(Pid, Monitor)),
    {{ok, added_and_logged}, maps:put(Name, User, Users)}.

-spec login(iolist(), iolist(), {pid(), reference()}, users()) -> {_, users()}.

login(Name, Password, {Pid, Monitor}, Users) ->
    case check_password(Name, Password, Users) of
        {ok, user_not_found} ->
            {{ok, user_not_found}, Users};
        {{ok, good_password}, User} ->
            NewUser = chat_user:login(User, chat_user:new_connection(Pid, Monitor)),
            NewUsers = maps:put(Name, NewUser, Users),
            {{ok, logged}, NewUsers};
        {{ok, bad_password}, _} ->
            {{ok, bad_password}, Users}
    end.

-spec get_rooms(rooms()) -> {ok, list()}.

get_rooms(Rooms) ->
    {ok, maps:keys(Rooms)}.

-spec join_room(iolist(), iolist(), users()) -> {_, users()}.

join_room(Name, RoomName, Users) ->
    % check room name
    case maps:find(Name, Users) of
        {ok, User} ->
            NewUser = chat_user:join_room(RoomName, User),
            {{ok, joined}, maps:put(Name, NewUser, Users)};
        error ->
            {{ok, user_not_found}, Users}
    end.

-spec quit_room(iolist(), iolist(), users()) -> {_, users()}.

quit_room(Name, RoomName, Users) ->
    % check room name
    case maps:find(Name, Users) of
        {ok, User} ->
            NewUser = chat_user:quit_room(RoomName, User),
            {{ok, quited}, maps:put(Name, NewUser, Users)};
        error ->
            {{ok, user_not_found}, Users}
    end.

-spec change_room(iolist(), iolist(), users()) -> {_, users()}.

change_room(Name, RoomName, Users) ->
    % check room name
    case maps:find(Name, Users) of
        {ok, User} ->
            NewUser = chat_user:change_room(RoomName, User),
            {{ok, changed}, maps:put(Name, NewUser, Users)};
        error ->
            {{ok, user_not_found}, Users}
    end.

-spec load_history(iolist(), users(), rooms()) -> {_, _ | list()}.

load_history(Name, Users, Rooms) ->
    case maps:find(Name, Users) of
        {ok, User} ->
            RoomName = chat_user:current_room(User),
            case maps:find(RoomName, Rooms) of
                {ok, Room} ->
                    {ok, chat_room:get_history(Room)};
                error ->
                    {ok, room_not_found}
            end;
        error ->
            {ok, user_not_found}
    end.

-spec send_message(iolist(), iolist(), users(), rooms()) -> {_, list(), users(), rooms()}.

send_message(Name, Message, Users, Rooms) ->
    case maps:find(Name, Users) of
        {ok, User} ->
            RoomName = chat_user:current_room(User),
            send_message_to_room(RoomName, format_message(Name, Message), Users, Rooms);
        error ->
            {{ok, user_not_found}, [], Users, Rooms}
    end.

-spec send_message_to_room(iolist(), iolist(), users(), rooms()) -> {_, list(), users(), rooms()}.

send_message_to_room(RoomName, Message, Users, Rooms) ->
    case maps:find(RoomName, Rooms) of
        {ok, Room} ->
            NewRoom = chat_room:add_message(Message, Room),
            NewRooms = maps:put(RoomName, NewRoom, Rooms),
            UsersInRoom = maps:filter(
                fun(_, SomeUser) ->
                    chat_user:in_room(RoomName, SomeUser)
                end, Users),
            Pids = maps:fold(
                fun(_, SomeUser, PidList) ->
                    [chat_user:get_pid(SomeUser) | PidList]
                end, [], UsersInRoom),
            {{ok, start_resend}, Message, Pids, Users, NewRooms};
        error ->
            {{ok, room_not_found}, [], [], Users, Rooms}
    end.

-spec get_monitor(iolist(), users()) -> {ok, user_not_found | reference()}.

get_monitor(Name, Users) ->
    case maps:find(Name, Users) of
        {ok, User} ->
            {ok, chat_user:get_monitor(User)};
        error ->
            {ok, user_not_found}
    end.

format_message(Name, Text) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(erlang:timestamp()),
    StrTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),
    erlang:iolist_to_binary([Name, " " ,StrTime, "\n\t", Text]).



