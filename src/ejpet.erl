-module(ejpet).
-author('nicolas.michel.lava@gmail.com').

-export([decode/2, encode/2,
         generator/1,
         compile/1, compile/2, compile/3, compile/4,
         backend/1,
         run/2, run/3,
         match/2, match/3, match/4,
         get_status/1,
         get_captures/1,
         get_capture/2, get_capture/3,
         empty_capture_set/0, empty_capture_set/1]).


-define(DEFAULT_BACKEND, jsx).
-define(DEFAULT_OPTIONS, []).
-define(DEFAULT_CACHE_FUN, (ejpet_default_cache:build_cache_fun())).


%% -----
%% Generator options
%% {number_strict_match, (true|false)}
%% {string_apply_escape_sequence, (true|false)}
%% -----

decode(JSON, Backend) when is_list(JSON) ->
    decode(list_to_binary(JSON), Backend);
decode(JSON, Backend) when is_binary(JSON) ->
    Backend:decode(JSON).

encode(Node, Backend) ->
    case Backend:encode(Node) of
        R when is_binary(R) ->
            R;
        R when is_list(R) ->
            list_to_binary(R)
    end.

generator(Backend) when is_atom(Backend) ->
    list_to_atom("ejpet_" ++ atom_to_list(Backend) ++ "_generators").

compile(Pattern) ->
    compile(Pattern, ?DEFAULT_BACKEND, ?DEFAULT_OPTIONS, ?DEFAULT_CACHE_FUN).

compile(Pattern, Backend) ->
    compile(Pattern, Backend, ?DEFAULT_OPTIONS, ?DEFAULT_CACHE_FUN).

compile(Pattern, Backend, Options) ->
    compile(Pattern, Backend, Options, ?DEFAULT_CACHE_FUN).

compile(Pattern, Backend, Options, CacheFun) ->
    {[], AST} = ejpet_parser:parse(ejpet_scanner:tokenize(Pattern, Options)),
    {ejpet, Backend, ejpet_generator:generate_matcher(AST, Options, (generator(Backend)), CacheFun)}.

backend({ejpet, Backend, _Fun}) ->
    Backend.

run(Node, EPM) ->
    run(Node, EPM, []).

run(Node, {ejpet, _Backend, Fun}, Params) ->
    Fun(Node, Params).

match(Subject, Pattern) ->
    match(Subject, Pattern, ?DEFAULT_OPTIONS, []).

match(Subject, Pattern, Options) ->
    match(Subject, Pattern, Options, []).

match(Subject, Pattern, Options, Params) when is_list(Subject) ->
    match(list_to_binary(Subject), Pattern, Options, Params);
match(Subject, {ejpet, Backend, Fun}, _Options, Params) ->
    Node = decode(Subject, Backend),
    {Status, Captures} = Fun(Node, Params),
    {Status, encode(Captures, Backend)};
match(Subject, Pattern, Options, Params) ->
    Opaque = compile(Pattern, ?DEFAULT_BACKEND, Options),
    match(Subject, Opaque, Options, Params).

get_status({S, _Caps}) when S == true; S == false->
    S.

get_captures({_S, Caps}) ->
    Caps.

get_capture(R, Name) ->
    get_capture(R, Name, ?DEFAULT_BACKEND).

get_capture(R, Name, Backend) when is_list(Name) ->
    get_capture(R, unicode:characters_to_binary(Name, utf8, utf8), Backend);
get_capture({_S, Caps}, Name, Backend) ->
    get_capture_any(Caps, Name, Backend).

empty_capture_set() ->
    empty_capture_set(?DEFAULT_BACKEND).

empty_capture_set(jsx) ->
    [{}];
empty_capture_set(jiffy) ->
    {[]};
empty_capture_set(mochijson2) ->
    {struct, []}.

get_capture_any(Caps, Name, jsx) ->
    get_capture_jsx(Caps, Name);
get_capture_any(Caps, Name, jiffy) ->
    get_capture_jiffy(Caps, Name);
get_capture_any(Caps, Name, mochijson2) ->
    get_capture_mochijson2(Caps, Name);
get_capture_any(Caps, Name, JPM) ->
    get_capture_any(Caps, Name, ejpet:backend(JPM)).

get_capture_jsx(Caps, Name) ->
    get_capture_kv(Caps, Name).

get_capture_jiffy({Caps}, Name) ->
    get_capture_kv(Caps, Name).
    
get_capture_mochijson2({struct, Caps}, Name) ->
    get_capture_kv(Caps, Name).

get_capture_kv([], _Name) ->
    not_found;
get_capture_kv([{Name, Values} | _], Name) ->
    {ok, Values};
get_capture_kv([{_N, _V} | Tail], Name) ->
    get_capture_kv(Tail, Name).
