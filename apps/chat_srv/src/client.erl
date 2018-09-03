-module (client).

-export ([user_check_password/2]).
-export ([user_new/3]).
-export ([user_login/2]).
-export ([user_logout/1]).
-export ([user_join_room/2]).
-export ([user_quit_room/2]).
-export ([user_change_room/2]).
-export ([user_send_message/3]).
-export ([user_receive_message/2]).
-export ([user_load_history/2]).
-export ([user_get_index/1]).
-export ([user_get_rooms/1]).

-record (user_state, {online, offline}).
-record (user, {name,
                pw,
                rms = [],
                current_rm,
                msg_index = 0,
                state = offline :: user_state(),
                tcp_pid :: pid()}).

-type user() :: #user{}.
-type user_state() :: #user_state{}.

-export_type([user/0]).

user_check_password(Password, User) ->
    case User#user.pw =:= Password of
        true ->
            {ok, good_password};
        _ ->
            {ok, bad_password}
    end.

user_new(Name, Password, Pid) ->
    #user{name = Name, pw = Password, state = online, tcp_pid = Pid}.

user_login(User, Pid) ->
    User#user{state = online, tcp_pid = Pid}.

user_logout(User) ->
    User#user{state = offline}.

user_get_index(User) ->
    User#user.msg_index.

user_get_rooms(User) ->
    User#user.rms.

user_join_room(RoomName, #user{rms = Rooms} = User) ->
    case lists:member(RoomName, Rooms) of
        true -> User;
        _ -> User#user{rms = [RoomName | rms]}
    end.

user_quit_room(RoomName, #user{rms = Rooms} = User) ->
    case lists:member(RoomName, Rooms) of
        false -> User;
        _ -> User#user{rms = lists:delete(RoomName, Rooms)}
    end.

user_change_room(RoomName, #user{rms = Rooms} = User) ->
    case lists:member(RoomName, Rooms) of
        false -> {{ok, room_not_found}, User};
        _ -> User#user{rms = lists:delete(RoomName, Rooms)}
    end.

user_send_message(_User, _Msg, _State) -> ok.
user_receive_message(_User, _Msg) -> ok.

user_load_history(_User, _State) -> ok.



