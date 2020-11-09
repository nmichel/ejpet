-module(ejpet_jiffy_codec).
-author('nicolas.michel.lava@gmail.com').

-export([decode/1, encode/1]).

decode(Node) ->
    jiffy:decode(Node).

encode(Node) ->
    jiffy:encode(Node).
