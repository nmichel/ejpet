-module(ejpet_memoize).
-author('nicolas.michel.lava@gmail.com').

-export([build_key/1]).

-define(STOB(S), <<(erlang:phash2(S)):4/little-signed-integer-unit:8>>).
-define(BS(S, T), <<S, (T)/binary>>).
-define(BB(B, T), <<(B)/binary, (T)/binary>>).

build_key({capture, {_AST, Hash}, Name}) ->
    ?BB(?BS("c", Hash), ?STOB(Name));
build_key(true) ->
    <<"t">>;
build_key(false) ->
    <<"f">>;
build_key(null) ->
    <<"n">>;
build_key({number, Value}) ->
    ?BS("f", ?STOB(Value));
build_key({string, String}) ->
    ?BS("s", ?STOB(String));
build_key({regex, String}) ->
    ?BS("r", ?STOB(String));
build_key(any) ->
    <<"a">>;
build_key({inject, Type, Name}) ->
    ?BB(?BS("i", ?STOB(Type)), ?STOB(Name));
build_key({object, any}) ->
    <<"oa">>;
build_key({object, Acc}) ->
    ?BS("o", fold_expr_list(Acc));
build_key({pair, {_KeyAST, KeyHash}, any}) ->
    ?BB(?BS("ps", ?STOB(KeyHash)), ?STOB("any"));
build_key({pair, {_KeyAST, KeyHash}, {_ValueAST, ValueHash}}) ->
    ?BB(?BS("ps", ?STOB(KeyHash)), ValueHash);
build_key({pair, any, {_ValueAST, ValueHash}}) ->
    ?BS("pa", ValueHash);
build_key({list, empty}) ->
    <<"le">>;
build_key({list, any}) ->
    <<"la">>;
build_key({find, {_AST, Hash}}) ->
    ?BS("f", Hash);
build_key(eol) ->
    <<"e">>;
build_key({span, Acc}) ->
    ?BS("ls", fold_expr_list(Acc));
build_key({span, Acc, eol}) ->
    ?BB(build_key({span, Acc}), build_key(eol));
build_key({list, Acc}) ->
    ?BS("l", fold_expr_list(Acc));
build_key({iterable, any}) ->
    <<"iz">>;
build_key({iterable, Acc, true}) ->
    ?BS("ig", fold_expr_list(Acc));
build_key({iterable, Acc, false}) ->
    ?BS("i", fold_expr_list(Acc));
build_key({descendant, Acc, true}) ->
    ?BS("dg", fold_expr_list(Acc));
build_key({descendant, Acc, false}) ->
    ?BS("d", fold_expr_list(Acc)).

fold_expr_list(Ks) ->
    lists:foldl(fun({_, HE}, A)->
                        <<A/binary, HE/binary>>
                end, <<"">>, Ks).
