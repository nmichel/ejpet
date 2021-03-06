-module(ejpet_validation_tests).
-author('nicolas.michel.lava@gmail.com').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(BACKENDS, [jsx, jiffy, mochijson2, jsone]).
-define(REF_BACKEND, jsx).

validation_test_() ->
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(inets),
    
    {ok, {_, _, Body}} = httpc:request(get, {"https://gist.githubusercontent.com/nmichel/8b0d6f194e89abb7281d/raw/907027e8d0be034433e1f56661a6a4fa3292daff/validation_tests.json", []}, [], [{body_format, binary}]),
    %%{ok, Body} = file:read_file("/home/ubuntu/tmp/validation_tests.json.git/validation_tests.json"),
    JSONTests = ejpet:decode(Body, ?REF_BACKEND),
    lists:foldl(fun(Backend, Acc) ->
            generate_test_suite_for_backend(Backend, JSONTests) ++ Acc
    end, [], ?BACKENDS).

generate_test_suite_for_backend(Backend, JSONTests) ->
    lists:reverse(lists:foldl(fun([{<<"pattern">>, Pattern}, {<<"tests">>, TestSet}], Acc) ->
        generate_test_suite(Backend, Pattern, TestSet, Acc)
    end, [], JSONTests)).

generate_test_suite(Backend, Pattern, TestSuite, Acc) ->
    M = ejpet:compile(Pattern, Backend),
    BackendBinary = list_to_binary(atom_to_list(Backend)),
    lists:foldl(fun(T, A) ->
        generate_test(<<Pattern/binary, $|, BackendBinary/binary>>, M, T, A)
    end, Acc, TestSuite).

generate_test(Name, Matcher, [{<<"inject">>, I}, {<<"node">>, N}, {<<"status">>, S}, {<<"captures">>, C}], Acc) ->
    [do_test(Name, Matcher, N, I, S, C) | Acc];
generate_test(Name, Matcher, [{<<"node">>, N}, {<<"status">>, S}, {<<"captures">>, C}], Acc) ->
    [do_test(Name, Matcher, N, [], S, C) | Acc].

do_test(Name, Matcher, Node, Injected, ExpS, ExpC) ->
    NodeText = jsx:encode(Node),
    NodeBackend = ejpet:decode(NodeText, ejpet:backend(Matcher)),
    {S, Caps} = ejpet:run(NodeBackend, Matcher, Injected),
    RealC = ejpet:decode(ejpet:encode(Caps, ejpet:backend(Matcher)), ejpet:backend(Matcher)),
    ReencodedExpC = ejpet:decode(ejpet:encode(ExpC, ?REF_BACKEND), ejpet:backend(Matcher)),

    % ?debugFmt("-----", []),
    % ?debugFmt("Name ~p", [unicode:characters_to_list(Name)]),
    % ?debugFmt("Test ~p", [unicode:characters_to_list(NodeText)]),
    % ?debugFmt("ExpC ~p", [ExpC]),
    % ?debugFmt("ReencodedExpC ~p", [ReencodedExpC]),
    % ?debugFmt("Caps ~p", [Caps]),
    % ?debugFmt("RealC ~p", [RealC]),
    % ?debugFmt("{~p, RealC} == {~p, ExpC} : ~p", [S, ExpS, {S, RealC} == {ExpS, ReencodedExpC}]),

    {<<Name/binary, $|, NodeText/binary>>, ?_test(?assert({S, RealC} == {ExpS, ReencodedExpC}))}.

-endif.

