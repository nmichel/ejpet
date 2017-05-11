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

-type backend() :: jsx | jiffy | mochijson2 | jsone.
-export_type([backend/0]).

-type jsx_term() :: any().
-type jiffy_term() :: any().
-type mochijson2_term() :: any().
-type jsone_term() :: any().
-type json_term() :: jsx_term() | jiffy_term() | mochijson2_term() | jsone_term().
-export_type([json_term/0]).

-type match_stat() :: boolean().
-export_type([match_stat/0]).

-type match_bindings() :: binary().
-type match_res() :: {match_stat(), match_bindings()}.
-export_type([match_res/0, match_bindings/0]).

-type run_param_value() :: true | false | number | binary() | re:mp().
-type run_param() :: {binary(), run_param_value()}.
-type run_params() :: list(run_params()).
-export_type([run_param/0, run_params/0]).

-type compile_option() :: {string_apply_escape_sequence, boolean()}
                        | {number_strict_match, boolean()}.
-type compile_options() :: list(compile_option()).
-export_type([compile_option/0, compile_options/0]).

-opaque matching_fn() :: fun((json_term(), run_params()) -> match_res()).
-opaque epm() :: {ejpet, backend(), matching_fn()}.
-export_type([epm/0, matching_fn/0]).

-type run_bindings() :: json_term().
-type run_res() :: {match_stat(), run_bindings()}.
-export_type([run_res/0, run_bindings/0]).

-opaque cache_key() :: binary().
-type cache_fun() :: fun(({get, cache_key()} | {store, cache_key(), matching_fn()}) -> false | {true, matching_fn()} | error | {ok, matching_fn()}).
-export_type([cache_key/0, cache_fun/0]).

-type json_input() :: binary() | string().
-export_type([json_input/0]).

-spec decode(json_input(), backend()) -> json_term().
decode(JSON, Backend) when is_list(JSON) ->
    decode(list_to_binary(JSON), Backend);
decode(JSON, Backend) when is_binary(JSON) ->
    Backend:decode(JSON).

-spec encode(json_term(), backend()) -> binary().
encode(Node, Backend) ->
    case Backend:encode(Node) of
        R when is_binary(R) ->
            R;
        R when is_list(R) ->
            list_to_binary(R)
    end.

-spec generator(backend()) -> atom().
generator(Backend) when is_atom(Backend) ->
    list_to_atom("ejpet_" ++ atom_to_list(Backend) ++ "_generators").

-spec compile(binary()) -> epm().
compile(Pattern) ->
    compile(Pattern, ?DEFAULT_BACKEND, ?DEFAULT_OPTIONS, ?DEFAULT_CACHE_FUN).

-spec compile(binary(), backend()) -> epm().
compile(Pattern, Backend) ->
    compile(Pattern, Backend, ?DEFAULT_OPTIONS, ?DEFAULT_CACHE_FUN).

-spec compile(binary(), backend(), compile_options()) -> epm().
compile(Pattern, Backend, Options) ->
    compile(Pattern, Backend, Options, ?DEFAULT_CACHE_FUN).

-spec compile(binary(), backend(), compile_options(), cache_fun()) -> epm().
compile(Pattern, Backend, Options, CacheFun) ->
    {[], AST} = ejpet_parser:parse(ejpet_scanner:tokenize(Pattern, Options)),
    {ejpet, Backend, ejpet_generator:generate_matcher(AST, Options, (generator(Backend)), CacheFun)}.

-spec backend(epm()) -> backend().
backend({ejpet, Backend, _Fun}) ->
    Backend.

-spec run(json_term(), epm()) -> run_res().
run(Node, EPM) ->
    run(Node, EPM, []).

-spec run(json_term(), epm(), run_params()) -> run_res().
run(Node, {ejpet, _Backend, Fun}, Params) ->
    Fun(Node, Params).

-spec match(json_input(), epm() | binary()) -> match_res().
match(Subject, Pattern) ->
    match(Subject, Pattern, ?DEFAULT_OPTIONS, []).

-spec match(json_input(), epm() | binary(), compile_options()) -> match_res().
match(Subject, Pattern, Options) ->
    match(Subject, Pattern, Options, []).

-spec match(json_input(), epm() | binary(), compile_options(), run_params()) -> match_res().
match(Subject, Pattern, Options, Params) when is_list(Subject) ->
    match(list_to_binary(Subject), Pattern, Options, Params);
match(Subject, {ejpet, Backend, Fun}, _Options, Params) ->
    Node = decode(Subject, Backend),
    {Status, Captures} = Fun(Node, Params),
    {Status, encode(Captures, Backend)};
match(Subject, Pattern, Options, Params) ->
    Opaque = compile(Pattern, ?DEFAULT_BACKEND, Options),
    match(Subject, Opaque, Options, Params).

-spec get_status(match_res() | run_res()) -> boolean().
get_status({S, _Caps}) when S == true; S == false->
    S.

-spec get_captures(match_res() | run_res()) -> json_term() | binary().
get_captures({_S, Caps}) ->
    Caps.

-spec get_capture(run_res(), binary() | string()) -> json_term().
get_capture(R, Name) ->
    get_capture(R, Name, ?DEFAULT_BACKEND).

-spec get_capture(run_res(), binary() | string(), backend()) -> json_term().
get_capture(R, Name, Backend) when is_list(Name) ->
    get_capture(R, unicode:characters_to_binary(Name, utf8, utf8), Backend);
get_capture({_S, Caps}, Name, Backend) ->
    get_capture_any(Caps, Name, Backend).

-spec empty_capture_set() -> json_term().
empty_capture_set() ->
    empty_capture_set(?DEFAULT_BACKEND).

-spec empty_capture_set(backend()) -> json_term().
empty_capture_set(jsx) ->
    [{}];
empty_capture_set(jiffy) ->
    {[]};
empty_capture_set(mochijson2) ->
    {struct, []};
empty_capture_set(jsone) ->
    [{}].

get_capture_any(Caps, Name, jsx) ->
    get_capture_jsx(Caps, Name);
get_capture_any(Caps, Name, jiffy) ->
    get_capture_jiffy(Caps, Name);
get_capture_any(Caps, Name, mochijson2) ->
    get_capture_mochijson2(Caps, Name);
get_capture_any(Caps, Name, jsone) ->
    get_capture_jsone(Caps, Name);
get_capture_any(Caps, Name, JPM) ->
    get_capture_any(Caps, Name, ejpet:backend(JPM)).

get_capture_jsx(Caps, Name) ->
    get_capture_kv(Caps, Name).

get_capture_jiffy({Caps}, Name) ->
    get_capture_kv(Caps, Name).
    
get_capture_mochijson2({struct, Caps}, Name) ->
    get_capture_kv(Caps, Name).
    
get_capture_jsone(Caps, Name) ->
    get_capture_kv(Caps, Name).

get_capture_kv([], _Name) ->
    not_found;
get_capture_kv([{Name, Values} | _], Name) ->
    {ok, Values};
get_capture_kv([{_N, _V} | Tail], Name) ->
    get_capture_kv(Tail, Name).
