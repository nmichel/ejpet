-module(ejpet_jsx_codec).
-author('nicolas.michel.lava@gmail.com').

-export([decode/1, encode/1]).

decode(Node) ->
    jsx:decode(Node, [{return_maps, false}]).

encode(Node) ->
    jsx:encode(Node).
