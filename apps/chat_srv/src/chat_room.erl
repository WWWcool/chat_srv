-module (chat_room).

-export ([add/1]).
-export ([fetch/3]).

-record (rm, {name, msg_buffer = [], msg_index = 0}).

fetch(Index, UserRooms, Rooms) ->
    ok.
add(Name) -> #rm{name = Name}.
send(_Msg, _State) -> ok.
get_buffer(_Room) -> ok.
get_new_count(_Room) -> ok.

