-module (room).

-record (rm, {name, users, msg_buffer, msg_index}).


rm_add(_Name, _State) -> ok.
rm_send(_Msg, _State) -> ok.
rm_get_buffer(_Room) -> ok.
rm_get_new_count(_Room) -> ok.

