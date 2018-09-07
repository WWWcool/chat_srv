-module(json_proto).

-export([decode/1]).
-export([encode/1]).

decode(Json) ->
    jiffy:decode(Json).

encode({Type, Data}) ->
    jiffy:encode(#{type => Type, data => Data}).
