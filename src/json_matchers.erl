-module(json_matchers).

-export([match_any/1,
         match_true/1,
         match_false/1,
         match_null/1,
         match_any_object/1,
         match_empty_list/1,
         match_any_list/1,
         match_any_iterable/1]).


match_any(_) ->
    true.

match_true(true) ->
    true;
match_true(_) ->
    false.

match_false(false) ->
    true;
match_false(_) ->
    false.

match_null(null) ->
    true;
match_null(_) ->
    false.

match_any_object([{}]) -> %% jsx special form of empty object
    true;
match_any_object([{_K, _V} | _Tail]) -> %% jsx non empty object
    true;
match_any_object(_) -> %% jsx non object
    false.

match_empty_list([]) ->
    true;
match_empty_list(_) ->
    false.

match_any_list([]) ->
    true;
match_any_list([_Head | _Tail]) ->
    true;
match_any_list(_) ->
    false.

match_any_iterable([{}]) -> %% jsx special form of empty object
    true;
match_any_iterable([{_K, _V} | _Tail]) -> %% jsx non empty object
    true;
match_any_iterable([]) ->
    true;
match_any_iterable([_Head | _Tail]) ->
    true;
match_any_iterable(_) -> %% jsx neither object nor list
    false.
