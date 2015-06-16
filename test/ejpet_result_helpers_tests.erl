-module(ejpet_result_helpers_tests).
-author('nicolas.michel.lava@gmail.com').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(BACKENDS, [jsx, jiffy, mochijson2]).
-define(REF_BACKEND, jsx).

status_true_test_() ->
    ?_test(?assert(ejpet:get_status({true, 42}) == true)).

status_false_test_() ->
    ?_test(?assert(ejpet:get_status({false, 42}) == false)).

status_other_test_() ->
    ?_test(?assertException(error, function_clause, ejpet:get_status({toto, 42}))).

status_pass_test_() ->
    JPM = ejpet:compile("[*]"),
    R = ejpet:run(ejpet:decode("[]", jsx), JPM),
    ?_test(?assert(ejpet:get_status(R) == true)).

status_fail_test_() ->
    JPM = ejpet:compile("[*]"),
    R = ejpet:run(ejpet:decode("{}", jsx), JPM),
    ?_test(?assert(ejpet:get_status(R) == false)).

capture_empty_default_test_() ->
    JPM = ejpet:compile("[*]"),
    R = ejpet:run(ejpet:decode("[]", ejpet:backend(JPM)), JPM),
    ?_test(?assert(ejpet:empty_capture_set() == ejpet:get_captures(R))).

capture_empty_jsx_test_() ->
    JPM = ejpet:compile("[*]", jsx),
    R = ejpet:run(ejpet:decode("[]", jsx), JPM),
    ?_test(?assert(ejpet:empty_capture_set(jsx) == ejpet:get_captures(R))).

capture_empty_jiffy_test_() ->
    JPM = ejpet:compile("[*]", jiffy),
    R = ejpet:run(ejpet:decode("[]", jiffy), JPM),
    ?_test(?assert(ejpet:empty_capture_set(jiffy) == ejpet:get_captures(R))).

capture_empty_mochijson2_test_() ->
    JPM = ejpet:compile("[*]", mochijson2),
    R = ejpet:run(ejpet:decode("[]", mochijson2), JPM),
    ?_test(?assert(ejpet:empty_capture_set(mochijson2) == ejpet:get_captures(R))).

capture_empty_jsx_auto_test_() ->
    JPM = ejpet:compile("[*]", jsx),
    R = ejpet:run(ejpet:decode("[]", ejpet:backend(JPM)), JPM),
    ?_test(?assert(ejpet:empty_capture_set(ejpet:backend(JPM)) == ejpet:get_captures(R))).

capture_empty_jiffy_auto_test_() ->
    JPM = ejpet:compile("[*]", jiffy),
    R = ejpet:run(ejpet:decode("[]", ejpet:backend(JPM)), JPM),
    ?_test(?assert(ejpet:empty_capture_set(ejpet:backend(JPM)) == ejpet:get_captures(R))).

capture_empty_mochijson2_auto_test_() ->
    JPM = ejpet:compile("[*]", mochijson2),
    R = ejpet:run(ejpet:decode("[]", ejpet:backend(JPM)), JPM),
    ?_test(?assert(ejpet:empty_capture_set(ejpet:backend(JPM)) == ejpet:get_captures(R))).

get_capture_default_test_() ->
    JPM = ejpet:compile("<[(?<f>_), *, (?<l>_)]>/g"),
    R = ejpet:run(ejpet:decode(<<"[[1, 2], [true, false]]">>, ejpet:backend(JPM)), JPM),
    [?_test(?assert(ejpet:get_capture(R, "f") == {ok, [1, true]})),
     ?_test(?assert(ejpet:get_capture(R, <<"f">>) == {ok, [1, true]})),
     ?_test(?assert(ejpet:get_capture(R, "l") == {ok, [2, false]})),
     ?_test(?assert(ejpet:get_capture(R, <<"l">>) == {ok, [2, false]}))].

get_capture_jsx_test_() ->
    JPM = ejpet:compile("<[(?<f>_), *, (?<l>_)]>/g", jsx),
    R = ejpet:run(ejpet:decode(<<"[[1, 2], [true, false]]">>, jsx), JPM),
    [?_test(?assert(ejpet:get_capture(R, "f", jsx) == {ok, [1, true]})),
     ?_test(?assert(ejpet:get_capture(R, <<"f">>, jsx) == {ok, [1, true]})),
     ?_test(?assert(ejpet:get_capture(R, "l", jsx) == {ok, [2, false]})),
     ?_test(?assert(ejpet:get_capture(R, <<"l">>, jsx) == {ok, [2, false]}))].

get_capture_jiffy_test_() ->
    JPM = ejpet:compile("<[(?<f>_), *, (?<l>_)]>/g", jiffy),
    R = ejpet:run(ejpet:decode(<<"[[1, 2], [true, false]]">>, jiffy), JPM),
    [?_test(?assert(ejpet:get_capture(R, "f", jiffy) == {ok, [1, true]})),
     ?_test(?assert(ejpet:get_capture(R, <<"f">>, jiffy) == {ok, [1, true]})),
     ?_test(?assert(ejpet:get_capture(R, "l", jiffy) == {ok, [2, false]})),
     ?_test(?assert(ejpet:get_capture(R, <<"l">>, jiffy) == {ok, [2, false]}))].

get_capture_mochijson2_test_() ->
    JPM = ejpet:compile("<[(?<f>_), *, (?<l>_)]>/g", mochijson2),
    R = ejpet:run(ejpet:decode(<<"[[1, 2], [true, false]]">>, mochijson2), JPM),
    [?_test(?assert(ejpet:get_capture(R, "f", mochijson2) == {ok, [1, true]})),
     ?_test(?assert(ejpet:get_capture(R, <<"f">>, mochijson2) == {ok, [1, true]})),
     ?_test(?assert(ejpet:get_capture(R, "l", mochijson2) == {ok, [2, false]})),
     ?_test(?assert(ejpet:get_capture(R, <<"l">>, mochijson2) == {ok, [2, false]}))].

-endif.
