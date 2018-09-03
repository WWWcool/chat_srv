-module (room).

-export ([rm_add/1]).
-export ([rm_fetch/3]).

-record (rm, {name, msg_buffer = [], msg_index = 0}).

rm_fetch(Index, UserRooms, Rooms) ->
    ok.
rm_add(Name) -> #rm{name = Name}.
rm_send(_Msg, _State) -> ok.
rm_get_buffer(_Room) -> ok.
rm_get_new_count(_Room) -> ok.

