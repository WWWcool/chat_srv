-module (chat_user).

-export ([check_password/2]).
-export ([new/3]).
-export ([login/2]).
-export ([logout/1]).
-export ([get_rooms/1]).
-export ([join_room/2]).
-export ([quit_room/2]).
-export ([change_room/2]).
-export ([current_room/1]).
-export ([in_room/2]).
-export ([get_pid/1]).
-export ([new_connection/2]).
-export ([get_monitor/1]).

-record (user, {name,
                pw,
                rms,
                current_rm,
                state = offline :: state(),
                connection :: connection()}).

-record (connection, {pid, monitor}).

-type user() :: #user{}.
-type state() :: online | offline.
-type connection() :: #connection{}.

-export_type([user/0]).

-spec check_password(iolist(), user()) -> {ok, good_password | bad_password}.

check_password(Password, User) ->
    case User#user.pw =:= Password of
        true ->
            {ok, good_password};
        false ->
            {ok, bad_password}
    end.

-spec new_connection(pid(), reference()) -> connection().

new_connection(Pid, Monitor) ->
    #connection{pid = Pid, monitor = Monitor}.

-spec new(iolist(), iolist(), connection()) -> user().

new(Name, Password, Connection) ->
    #user{name = Name, pw = Password, state = online, connection = Connection, rms = dict:new()}.

-spec login(user(), connection()) -> user().

login(User, Connection) ->
    User#user{state = online, connection = Connection}.

-spec logout(user()) -> user().

logout(User) ->
    User#user{state = offline}.

-spec get_rooms(user()) -> list().

get_rooms(#user{rms = Rooms}) ->
    dict:fetch_keys(Rooms).

-spec join_room(iolist(), user()) -> user().

join_room(RoomName, #user{rms = Rooms} = User) ->
    case dict:is_key(RoomName, Rooms) of
        true ->
            User#user{current_rm = RoomName};
        false ->
            User#user{rms = dict:store(RoomName, 0, Rooms), current_rm = RoomName}
    end.

-spec quit_room(iolist(), user()) -> user().

quit_room(RoomName, #user{rms = Rooms, current_rm = Room} = User) ->
    case dict:is_key(RoomName, Rooms) of
        false ->
            User;
        true ->
            NewUser = User#user{rms = dict:erase(RoomName, Rooms)},
            case Room =:= RoomName of
                true ->
                    NewUser#user{current_rm = undefined};
                false ->
                    NewUser
            end
    end.

-spec change_room(iolist(), user()) -> user().

change_room(RoomName, #user{rms = Rooms} = User) ->
    case dict:is_key(RoomName, Rooms) of
        false ->
            join_room(RoomName, User);
        true ->
            User#user{current_rm = RoomName}
    end.

-spec current_room(user()) -> iolist().

current_room(User) ->
    User#user.current_rm.

-spec in_room(iolist(), user()) -> boolean().

in_room(RoomName, #user{rms = Rooms}) ->
    dict:is_key(RoomName, Rooms).

-spec get_pid(user()) -> pid().

get_pid(User) ->
    (User#user.connection)#connection.pid.

-spec get_monitor(user()) -> reference().

get_monitor(User) ->
    (User#user.connection)#connection.monitor.

