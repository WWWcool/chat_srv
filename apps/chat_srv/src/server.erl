-module (server).

-export ([srv_init_users/0]).
-export ([srv_init_rooms/0]).
-export ([srv_disconnect/2]).
-export ([srv_check_name/2]).
-export ([srv_login/4]).
-export ([srv_new_user/4]).
-export ([srv_get_rooms/1]).
-export ([srv_fetch_history/3]).
-export ([srv_join_room/3]).
-export ([srv_quit_room/3]).
-export ([srv_change_room/3]).
-export ([srv_load_history/3]).
-export ([srv_send_message/4]).

srv_init_users() -> dict:new().
srv_init_rooms() -> dict:new().

srv_disconnect(Name, Users) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            NewUser = client:user_logout(User),
            {{ok, disconnected}, dict:append(Name, NewUser, Users)};
        error ->
            {{ok, user_not_found}, Users}
    end.

srv_check_name(Name, Users) ->
    case dict:find(Name, Users) of
        {ok, _} -> {ok, found};
        error -> {ok, user_not_found}
    end.

srv_check_password(Name, Password, Users) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            Replay = client:user_check_password(Password, User),
            {Replay, User};
        error -> {ok, user_not_found}
    end.

srv_new_user(Name, Password, Users, TCPPid) ->
    User = client:user_new(Name, Password, TCPPid),
    {{ok, user_added_and_logged}, dict:append(Name, User, Users)}.

srv_login(Name, Password, Users, TCPPid) ->
    case srv_check_password(Name, Password, Users) of
        {ok, user_not_found} ->
            {{ok, user_not_found}, Users};
        {{ok, good_password}, User} ->
            NewUser = client:user_login(User, TCPPid),
            NewUsers = dict:append(Name, NewUser, Users),
            {{ok, logged}, NewUsers};
        {{ok, bad_password}, _} ->
            {{ok, bad_password}, Users}
    end.

srv_get_rooms(Rooms) ->
    {ok, dict:fetch_keys(Rooms)}.

srv_fetch_history(Name, Users, Rooms) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            Index = client:user_get_index(User),
            UserRooms = client:user_get_rooms(User),

            Fun = fun (RoomName, Room, Acc) ->
                RoomIndex = room:rm_get_index(Room),
                case Index < RoomIndex of
                    true -> [{RoomName, RoomIndex - Index} | Acc];
                    _ -> Acc
                end
            end,

            {ok, dict:fold(Fun, [], dict:filter(fun(RoomName, _) ->
                    lists:member(RoomName, UserRooms) end, Rooms))};

        error -> {ok, user_not_found}
    end.

srv_join_room(Name, RoomName, Users) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            NewUser = client:user_join_room(RoomName, User),
            {{ok, joined}, dict:append(Name, NewUser, Users)};
        error -> {{ok, user_not_found}, Users}
    end.

srv_quit_room(Name, RoomName, Users) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            NewUser = client:user_quit_room(RoomName, User),
            {{ok, joined}, dict:append(Name, NewUser, Users)};
        error -> {{ok, user_not_found}, Users}
    end.

srv_change_room(Name, RoomName, Users) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            NewUser = client:user_change_room(RoomName, User),
            {{ok, joined}, dict:append(Name, NewUser, Users)};
        error -> {{ok, user_not_found}, Users}
    end.

srv_load_history(Name, Users, Rooms) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            RoomName = client:user_current_room(User),
            case dict:find(RoomName, Rooms) of
                {ok, Room} ->
                    {ok, room:rm_get_history(Room)};
                error ->
                    {ok, room_not_found}
            end;
        error -> {ok, user_not_found}
    end.

srv_send_message(Name, Message, Users, Rooms) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            RoomName = client:user_current_room(User),
            case dict:find(RoomName, Rooms) of
                {ok, Room} ->
                    Fun = fun(_, SomeUser) ->
                        UserRooms = client:user_get_rooms(SomeUser),
                        case lists:member(RoomName, UserRooms) of
                            true ->
                                client:user_send_message(Message, User);
                            _ ->
                                User
                        end
                    end,
                    NewUsers = dict:map(Fun, Users),
                    NewRooms = dict:append(RoomName, room:rm_add_message(Message, Room), Rooms),
                    {{ok, sended}, NewUsers, NewRooms};
                error ->
                    {{ok, room_not_found}, Users, Rooms}
            end;
        error ->
            {{ok, user_not_found}, Users, Rooms}
    end.

