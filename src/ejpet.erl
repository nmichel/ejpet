-module(ejpet).
-author('nicolas.michel.lava@gmail.com').

-export([compile/2,
         match/2,
         match/3]).


-define(DEFAULT_BACKEND, jsx).


compile(Pattern, Backend) ->
    {[], AST} = ejpet_parser:parse(ejpet_scanner:tokenize(Pattern)),
    (backend(Backend)):generate_matcher(AST).

match(Pattern, Node, Backend) ->
    Fun = compile(Pattern, Backend),
    Fun(decode(Backend, Node)).

match(Pattern, Node) ->
    match(Pattern, Node, ?DEFAULT_BACKEND).

%% -----

backend(Backend) ->
    list_to_atom("ejpet_" ++ atom_to_list(Backend) ++ "_generators").

decode(Backend, JSON) when is_binary(JSON) ->
    Backend:decode(JSON).

