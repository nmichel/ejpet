-module(ejpet_jsone_codec).
-author('nicolas.michel.lava@gmail.com').

-export([decode/1, encode/1]).

decode(Node) ->
    jsone:decode(Node).

encode(Node) ->
    jsone:encode(Node).
