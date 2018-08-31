-module (server).

-export ([srv_init/0]).
-export ([srv_check_name/2]).
-export ([srv_login/3]).
-export ([srv_new_user/3]).

srv_init() -> dict:new().

srv_check_name(Name, Users) ->
    case dict:find(Name, Users) of
        {ok, _} -> {ok, found};
        error -> {ok, not_found}
    end.

srv_check_password(Name, Password, Users) ->
    case dict:find(Name, Users) of
        {ok, User} ->
            Replay = user:user_check_password(Password, User),
            {Replay, User};
        error -> {ok, not_found}
    end.

srv_new_user(Name, Password, Users) ->
    User = user:user_new(Name, Password),
    {{ok, user_added_and_logged}, dict:append(Name, User, Users)}.

srv_login(Name, Password, Users) ->
    case srv_check_password(Name, Password, Users) of
        {ok, not_found} ->
            {{ok, not_found}, Users};
        {{ok, good_password}, User} ->
            NewUser = user:login(User),
            NewUsers = dict:append(Name, NewUser, Users),
            {{ok, logged}, NewUsers};
        {{ok, bad_password}, _} ->
            {{ok, bad_password}, Users}
    end.
