-module(json_proto).

-export([decode/1]).
-export([encode/1]).

decode(Json) ->
    jiffy:decode(Json).

encode(Tuple) ->
    jiffy:encode(erlang:term_to_binary(Tuple)).
