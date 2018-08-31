-module (user).

-export ([user_check_password/2]).
-export ([user_new/2]).
-export ([user_login/1]).
-export ([user_logout/1]).
-export ([user_join/3]).
-export ([user_quit/3]).
-export ([user_change/3]).
-export ([user_send_message/3]).
-export ([user_receive_message/2]).
-export ([user_load_history/2]).

-record (user_state, {online, offline}).
-record (user, {name, pw, rms = [], current_rm, state = offline :: user_state()}).

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

user_new(Name, Password) ->
    #user{name = Name, pw = Password, state = online}.

user_login(User) ->
    User#user{state = online}.

user_logout(User) ->
    User#user{state = offline}.


user_join(_User, _Room, _State) -> ok.
user_quit(_User, _Room, _State) -> ok.
user_change(_User, _Room, _State) -> ok.

user_send_message(_User, _Msg, _State) -> ok.
user_receive_message(_User, _Msg) -> ok.

user_load_history(_User, _State) -> ok.



