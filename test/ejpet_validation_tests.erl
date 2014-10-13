-module(ejpet_validation_tests).
-author('nicolas.michel.lava@gmail.com').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(BACKENDS, [jsx, jiffy, mochijson2]).
-define(REF_BACKEND, jsx).

validation_test_() ->
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(inets),
    
    {ok, {_, _, Body}} = httpc:request("http://gist.githubusercontent.com/nmichel/8b0d6f194e89abb7281d/raw/ef6ece35c53486f410deb0e735948d01bcd0f56e/validation_tests.json"),
%    {ok, Body} = file:read_file("/home/nmichel/projects/validation_tests.json.git/validation_tests.json"),
    JSONTests = ?REF_BACKEND:decode(list_to_binary(Body)),
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
    RealC = ejpet:decode(ejpet:encode(Caps, ejpet:backend(Matcher)), ?REF_BACKEND),
    {<<Name/binary, $|, NodeText/binary>>, ?_test(?assert({S, RealC} == {ExpS, ExpC}))}.

-endif.

