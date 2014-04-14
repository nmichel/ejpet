-module(ejpet).
-author('nicolas.michel.lava@gmail.com').

-export([decode/2, encode/2,
         generator/1,
         compile/1, compile/2, compile/3, compile/4,
         backend/1,
         run/2, run/3,
         match/2, match/3, match/4]).


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
    case Fun(Node, Params) of
        {true, CaptureList} ->
            {true, [{Name, [encode(Capture, Backend) || Capture <- Captures]} || {Name, Captures} <- CaptureList]};
        R ->
            R
    end;
match(Subject, Pattern, Options, Params) ->
    Opaque = compile(Pattern, ?DEFAULT_BACKEND, Options),
    match(Subject, Opaque, Options, Params).
