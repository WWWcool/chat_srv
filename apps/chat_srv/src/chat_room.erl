-module (chat_room).

-export ([add/1]).
-export ([get_history/1]).
-export ([add_message/2]).

-record (rm, {name, msg_buffer = [], msg_index = 0}).

add(Name) -> #rm{name = Name}.
get_history(Room) -> Room#rm.msg_buffer.
add_message(Message, #rm{msg_buffer = Buf} = Room) ->
    Room#rm{msg_buffer = [Message | Buf]}.
