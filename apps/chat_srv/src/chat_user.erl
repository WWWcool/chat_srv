-module (chat_user).

-export ([check_password/2]).
-export ([new/3]).
-export ([login/2]).
-export ([logout/1]).
-export ([join_room/2]).
-export ([quit_room/2]).
-export ([change_room/2]).
-export ([send_message/3]).
-export ([receive_message/2]).
-export ([load_history/2]).
-export ([get_index/1]).
-export ([get_rooms/1]).

-record (state, {online, offline}).
-record (user, {name,
                pw,
                rms = [],
                current_rm,
                msg_index = 0,
                state = offline :: state(),
                tcp_pid :: pid()}).

-type user() :: #user{}.
-type state() :: #state{}.

-export_type([user/0]).

check_password(Password, User) ->
    case User#user.pw =:= Password of
        true ->
            {ok, good_password};
        _ ->
            {ok, bad_password}
    end.

new(Name, Password, Pid) ->
    #user{name = Name, pw = Password, state = online, tcp_pid = Pid}.

login(User, Pid) ->
    User#user{state = online, tcp_pid = Pid}.

logout(User) ->
    User#user{state = offline}.

get_index(User) ->
    User#user.msg_index.

get_rooms(User) ->
    User#user.rms.

join_room(RoomName, #user{rms = Rooms} = User) ->
    case lists:member(RoomName, Rooms) of
        true -> User;
        _ -> User#user{rms = [RoomName | rms]}
    end.

quit_room(RoomName, #user{rms = Rooms} = User) ->
    case lists:member(RoomName, Rooms) of
        false -> User;
        _ -> User#user{rms = lists:delete(RoomName, Rooms)}
    end.

change_room(RoomName, #user{rms = Rooms} = User) ->
    case lists:member(RoomName, Rooms) of
        false -> {{ok, room_not_found}, User};
        _ -> User#user{rms = lists:delete(RoomName, Rooms)}
    end.

send_message(_User, _Msg, _State) -> ok.
receive_message(_User, _Msg) -> ok.

load_history(_User, _State) -> ok.



