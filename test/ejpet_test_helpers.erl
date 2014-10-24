-module(ejpet_test_helpers).
-author('nicolas.michel.lava@gmail.com').

-export([generate_test_list/1]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(BACKENDS, [jsx, jiffy, mochijson2]).
-define(REF_BACKEND, jsx).


generate_test_list(TestDescs) ->
    [generate_test_list(TestDescs, Backend) || Backend <- ?BACKENDS].

generate_test_list(TestDescs, Backend) ->
    lists:reverse(
      lists:foldl(fun({Pattern, T}, FnAcc) ->
                          %% Produce the matcher
                          %% 
                          {[], AST} = ejpet_parser:parse(ejpet_scanner:tokenize(Pattern, [])),
                          F = ejpet_generator:generate_matcher(AST, [], (ejpet:generator(Backend))),

                          BuildTest = fun(Node, Injected, Expected = {ExpStatus, _ExpCaptures}) ->
                                              PatternPart = unicode:characters_to_binary(Pattern, utf8, utf8),
                                              StatusAsBin = list_to_binary(atom_to_list(ExpStatus)),
                                              TestName = <<PatternPart/binary, " | ", Node/binary, " | ", StatusAsBin/binary>>,
                                              
                                              %% Execute the test
                                              %% 
                                              {Status, Captures} = F(ejpet:decode(Node, Backend), Injected),

                                              %% Transform captures to text
                                              %% 
                                              JSONCaptures = [{VarName, [ejpet:encode(Cap, Backend) || Cap <- Caps]} || {VarName, Caps} <- Captures],

                                              %% Parse again and stringify captures using the reference backend
                                              %% 
                                              RefCaptures = [{VarName, [ejpet:encode(ejpet:decode(Cap, ?REF_BACKEND), ?REF_BACKEND) || Cap <- Caps]} || {VarName, Caps} <- JSONCaptures],

                                              {TestName, ?_test(?assert({Status, RefCaptures} == Expected))}
                                      end,
                          lists:foldl(fun({Node, Expected}, Acc) ->
                                              [BuildTest(Node, [], Expected) | Acc];
                                         ({Node, Injected, Expected}, Acc) ->
                                              [BuildTest(Node, Injected, Expected) | Acc]
                                      end, FnAcc, T)
                  end, [], TestDescs)).

-endif.
