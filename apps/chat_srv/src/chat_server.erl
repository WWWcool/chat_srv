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

init_users() -> dict:new().
init_rooms() ->
    Rooms = dict:new(),
    LobbyName = "Lobby",
    Lobby = chat_room:add(LobbyName),
    dict:store(LobbyName, Lobby, Rooms).

disconnect(Name, Users) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            NewUser = chat_user:logout(User),
            {{ok, disconnected}, dict:store(Name, NewUser, Users)};
        error ->
            {{ok, user_not_found}, Users}
    end.

check_name(Name, Users) ->
    case dict:find(Name, Users) of
        {ok, _} ->
            {ok, found};
        error ->
            {ok, user_not_found}
    end.

check_password(Name, Password, Users) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            Reply = chat_user:check_password(Password, User),
            {Reply, User};
        error ->
            {ok, user_not_found}
    end.

new_user(Name, Password, TCPPid, Users) ->
    User = chat_user:new(Name, Password, TCPPid),
    {{ok, added_and_logged}, dict:store(Name, User, Users)}.

login(Name, Password, TCPPid, Users) ->
    case check_password(Name, Password, Users) of
        {ok, user_not_found} ->
            {{ok, user_not_found}, Users};
        {{ok, good_password}, User} ->
            NewUser = chat_user:login(User, TCPPid),
            NewUsers = dict:store(Name, NewUser, Users),
            {{ok, logged}, NewUsers};
        {{ok, bad_password}, _} ->
            {{ok, bad_password}, Users}
    end.

get_rooms(Rooms) ->
    {ok, dict:fetch_keys(Rooms)}.

join_room(Name, RoomName, Users) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            NewUser = chat_user:join_room(RoomName, User),
            {{ok, joined}, dict:store(Name, NewUser, Users)};
        error ->
            {{ok, user_not_found}, Users}
    end.

quit_room(Name, RoomName, Users) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            NewUser = chat_user:quit_room(RoomName, User),
            {{ok, quited}, dict:store(Name, NewUser, Users)};
        error ->
            {{ok, user_not_found}, Users}
    end.

change_room(Name, RoomName, Users) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            NewUser = chat_user:change_room(RoomName, User),
            {{ok, changed}, dict:store(Name, NewUser, Users)};
        error ->
            {{ok, user_not_found}, Users}
    end.

load_history(Name, Users, Rooms) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            RoomName = chat_user:current_room(User),
            case dict:find(RoomName, Rooms) of
                {ok, Room} ->
                    {ok, chat_room:get_history(Room)};
                error ->
                    {ok, room_not_found}
            end;
        error ->
            {ok, user_not_found}
    end.

send_message(Name, Message, Users, Rooms) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            RoomName = chat_user:current_room(User),
            send_message_to_room(RoomName, Message, Users, Rooms);
        error ->
            {{ok, user_not_found}, Users, Rooms}
    end.

send_message_to_room(RoomName, Message, Users, Rooms) ->
    case dict:find(RoomName, Rooms) of
        {ok, Room} ->
            NewRoom = chat_room:add_message(Message, Room),
            NewRooms = dict:store(RoomName, NewRoom, Rooms),
            UsersInRoom = dict:filter(
                fun(_, SomeUser) ->
                    chat_user:in_room(RoomName, SomeUser)
                end, Users),
            Pids = dict:fold(
                fun(_, SomeUser, PidList) ->
                    [chat_user:get_pid(SomeUser) | PidList]
                end, [], UsersInRoom),
            {{ok, start_resend}, Pids, Users, NewRooms};
        error ->
            {{ok, room_not_found}, [], Users, Rooms}
    end.







