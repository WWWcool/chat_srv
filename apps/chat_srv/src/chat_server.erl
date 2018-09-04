-module (chat_server).

-export ([init_users/0]).
-export ([init_rooms/0]).
-export ([disconnect/2]).
-export ([check_name/2]).
-export ([login/4]).
-export ([new_user/4]).
-export ([get_rooms/1]).
-export ([fetch_history/3]).
-export ([join_room/3]).
-export ([quit_room/3]).
-export ([change_room/3]).
-export ([load_history/3]).
-export ([send_message/4]).

init_users() -> dict:new().
init_rooms() -> dict:new().

disconnect(Name, Users) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            NewUser = chat_user:logout(User),
            {{ok, disconnected}, dict:append(Name, NewUser, Users)};
        error ->
            {{ok, not_found}, Users}
    end.

check_name(Name, Users) ->
    case dict:find(Name, Users) of
        {ok, _} -> {ok, found};
        error -> {ok, not_found}
    end.

check_password(Name, Password, Users) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            Replay = chat_user:check_password(Password, User),
            {Replay, User};
        error -> {ok, not_found}
    end.

new_user(Name, Password, Users, TCPPid) ->
    User = chat_user:new(Name, Password, TCPPid),
    {{ok, added_and_logged}, dict:append(Name, User, Users)}.

login(Name, Password, Users, TCPPid) ->
    case check_password(Name, Password, Users) of
        {ok, not_found} ->
            {{ok, not_found}, Users};
        {{ok, good_password}, User} ->
            NewUser = chat_user:login(User, TCPPid),
            NewUsers = dict:append(Name, NewUser, Users),
            {{ok, logged}, NewUsers};
        {{ok, bad_password}, _} ->
            {{ok, bad_password}, Users}
    end.

get_rooms(Rooms) ->
    {ok, dict:fetch_keys(Rooms)}.

fetch_history(Name, Users, Rooms) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            Index = chat_user:get_index(User),
            UserRooms = chat_user:get_rooms(User),

            Fun = fun (RoomName, Room, Acc) ->
                RoomIndex = chat_room:get_index(Room),
                case Index < RoomIndex of
                    true -> [{RoomName, RoomIndex - Index} | Acc];
                    _ -> Acc
                end
            end,

            {ok, dict:fold(Fun, [], dict:filter(fun(RoomName, _) ->
                    lists:member(RoomName, UserRooms) end, Rooms))};

        error -> {ok, not_found}
    end.

join_room(Name, RoomName, Users) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            NewUser = chat_user:join_room(RoomName, User),
            {{ok, joined}, dict:append(Name, NewUser, Users)};
        error -> {{ok, not_found}, Users}
    end.

quit_room(Name, RoomName, Users) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            NewUser = chat_user:quit_room(RoomName, User),
            {{ok, joined}, dict:append(Name, NewUser, Users)};
        error -> {{ok, not_found}, Users}
    end.

change_room(Name, RoomName, Users) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            NewUser = chat_user:change_room(RoomName, User),
            {{ok, joined}, dict:append(Name, NewUser, Users)};
        error -> {{ok, not_found}, Users}
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
        error -> {ok, not_found}
    end.

send_message(Name, Message, Users, Rooms) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            RoomName = chat_user:current_room(User),
            case dict:find(RoomName, Rooms) of
                {ok, Room} ->
                    Fun = fun(_, SomeUser) ->
                        UserRooms = chat_user:get_rooms(SomeUser),
                        case lists:member(RoomName, UserRooms) of
                            true ->
                                chat_user:send_message(Message, User);
                            _ ->
                                User
                        end
                    end,
                    NewUsers = dict:map(Fun, Users),
                    NewRooms = dict:append(RoomName, chat_room:add_message(Message, Room), Rooms),
                    {{ok, sended}, NewUsers, NewRooms};
                error ->
                    {{ok, room_not_found}, Users, Rooms}
            end;
        error ->
            {{ok, not_found}, Users, Rooms}
    end.

