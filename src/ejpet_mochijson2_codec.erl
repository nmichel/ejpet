-module(ejpet_mochijson2_codec).
-author('nicolas.michel.lava@gmail.com').

-export([decode/1, encode/1]).

decode(Node) ->
    mochijson2:decode(Node).

encode(Node) ->
    mochijson2:encode(Node).
