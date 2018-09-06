-module(json_proto).

-export([decode/1]).
-export([encode/1]).

decode(Tuple) ->
    jiffy:decode(erlang:term_to_binary(Tuple)).

encode(Json) ->
    erlang:binary_to_term(jiffy:encode(Json)).
