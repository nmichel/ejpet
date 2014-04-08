-module(ejpet_memoize).
-author('nicolas.michel.lava@gmail.com').

-export([build_key/1]).

-define(STOB(S), (list_to_binary(S))).
-define(BS(S, T), <<S, (T)/binary>>).
-define(BB(B, T), <<(B)/binary, (T)/binary>>).

build_key({capture, {_AST, Hash}, Name}) ->
    ?BB(?BS("capture", Hash), ?STOB(Name));
build_key(true) ->
    <<"true">>;
build_key(false) ->
    <<"false">>;
build_key(null) ->
    <<"null">>;
build_key({number, Value}) ->
    ?BS("number", ?STOB(lists:concat([Value])));
build_key({string, String}) ->
    ?BS("string", String);
build_key({regex, String}) ->
    ?BS("regex", String);
build_key({descendant, [{_AST, Hash}], true}) ->
    ?BS("descendantg", Hash);
build_key({descendant, [{_AST, Hash}], false}) ->
    ?BS("descendant", Hash);
build_key(any) ->
    <<"any">>;
build_key({inject, Type, Name}) ->
    ?BS("inject", ?STOB(lists:concat([Type, Name])));
build_key({object, any}) ->
    <<"objectany">>;
build_key({object, Acc}) ->
    ?BS("object", fold_key_list(Acc));
build_key({pair, {string, String}, any}) ->
    ?BB(?BS("pairstring", String), ?STOB("any"));
build_key({pair, {string, String}, {_AST, Hash}}) ->
    ?BB(?BS("pairstring", String), Hash);
build_key({pair, any, {_AST, Hash}}) ->
    ?BS("pairany", Hash);
build_key({list, empty}) ->
    <<"listempty">>;
build_key({list, any}) ->
    <<"listany">>;
build_key({find, {_AST, Hash}}) ->
    ?BS("find", Hash);
build_key(eol) ->
    <<"eol">>;
build_key({list, Acc}) ->
    ?BS("list", fold_key_list(Acc));
build_key({iterable, any}) ->
    <<"iterableany">>;
build_key({iterable, Acc, true}) ->
    ?BS("iterableg", fold_key_list(Acc));
build_key({iterable, Acc, false}) ->
    ?BS("iterable", fold_key_list(Acc)).

fold_key_list(Ks) ->
    lists:foldl(fun({_, HE}, A) ->
                        <<A/binary, HE/binary>>
                end, <<"">>, Ks).
