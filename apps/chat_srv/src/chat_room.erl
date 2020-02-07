-module (chat_room).

-export ([add/1]).
-export ([get_history/1]).
-export ([add_message/2]).

-record (rm, {name, msg_buffer = [], msg_index = 0}).

-type room() :: #rm{}.

-export_type([room/0]).

-spec add(iolist()) -> room().

add(Name) -> #rm{name = Name}.

-spec get_history(room()) -> list().

get_history(Room) -> Room#rm.msg_buffer.

-spec add_message(iolist(), room()) -> room().

add_message(Message, #rm{msg_buffer = Buf} = Room) ->
    Room#rm{msg_buffer = [Message | Buf]}.
