-module(ejpet_scanner).
-author('nicolas.michel.lava@gmail.com').

-export([tokenize/2]).


-define(is_hexa(C),
        (C >= $A andalso C =< $F) orelse
        (C >= $a andalso C =< $f) orelse
        (C >= $0 andalso C =< $9)).

-record(config, {
          string_apply_escape_sequence = true,
          string_raw = false
         }).


tokenize(Pattern, Options) when is_list(Pattern) ->
    tokenize(list_to_binary(Pattern), Options);
tokenize(Pattern, Options) ->
    Config = #config{string_apply_escape_sequence = proplists:get_value(string_apply_escape_sequence, Options, true),
                     string_raw = proplists:get_value(string_raw, Options, false)},
    tokenize(Pattern, [state_root], [], Config).

tokenize(<<>>, [state_root | _], Acc, _Config) ->
    lists:reverse(Acc);
tokenize(<<${, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [open_curvy_brace | Acc], Config);
tokenize(<<$}, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [close_curvy_brace | Acc], Config);
tokenize(<<$[, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [open_square_brace | Acc], Config);
tokenize(<<$], T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [close_square_brace | Acc], Config);
tokenize(<<$<, $!, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [open_angle_brace_bang | Acc], Config);
tokenize(<<$<, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [open_angle_brace | Acc], Config);
tokenize(<<$!, $>, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [close_angle_brace_bang | Acc], Config);
tokenize(<<$>, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [close_angle_brace | Acc], Config);
tokenize(<<$,, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [coma | Acc], Config);
tokenize(<<$:, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [column | Acc], Config);
tokenize(<<$_, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [underscore | Acc], Config);
tokenize(<<$*, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [star | Acc], Config);
tokenize(<<$(, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [open_paren | Acc], Config);
tokenize(<<$), T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [close_paren | Acc], Config);
tokenize(<<$/, $g, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [slash_g | Acc], Config);

tokenize(<<$\n, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, Acc, Config);
tokenize(<<$\r, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, Acc, Config);
tokenize(<<$\t, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, Acc, Config);
tokenize(<<$\s, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, Acc, Config);

tokenize(<<"true", T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [true | Acc], Config);
tokenize(<<"false", T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [false | Acc], Config);
tokenize(<<"null", T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [null | Acc], Config);
tokenize(<<"string", T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [string | Acc], Config);
tokenize(<<"number", T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [number | Acc], Config);
tokenize(<<"boolean", T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [boolean | Acc], Config);
tokenize(<<"regex", T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [regex | Acc], Config);

tokenize(<<$?, $<, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, [{state_capture, []} | State], Acc, Config);
tokenize(<<$#, $", T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, [{state_string, ""}, {state_pattern} | State], Acc, Config);
tokenize(<<$", T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, [{state_string, ""} | State], Acc, Config);
tokenize(<<$!, $<, T/binary>>, State = [state_root | _], Acc, Config) ->
    tokenize(T, [{state_capture, []}, state_inject | State], Acc, Config);

tokenize(<<$>, T/binary>>, [{state_capture, Name = [_|_]} | Tail], Acc, Config) ->
    tokenize(T, Tail, [{capture, lists:reverse(Name)} | Acc], Config);
tokenize(<<$_, T/binary>>, [{state_capture, Name} | Tail], Acc, Config) ->
    tokenize(T, [{state_capture, [$_ | Name]} | Tail], Acc, Config);
tokenize(<<V, T/binary>>, [{state_capture, Name} | Tail], Acc, Config) when V >= $A, V =< $Z ->
    tokenize(T, [{state_capture, [V | Name]} | Tail], Acc, Config);
tokenize(<<V, T/binary>>, [{state_capture, Name} | Tail], Acc, Config) when V >= $a, V =< $z ->
    tokenize(T, [{state_capture, [V | Name]} | Tail], Acc, Config);
tokenize(<<V, T/binary>>, [{state_capture, Name} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_capture, [V | Name]} | Tail], Acc, Config);

%% state_inject is never on top, except when the wrapped capture has been parsed.
%% We just have to transform the token "capture", into "inject", and we are done.
%% 
tokenize(T, [state_inject | Tail], [{capture, Name} | Acc], Config) ->
    tokenize(T, Tail, [{inject, Name} | Acc], Config);

tokenize(<<$-, V, T/binary>>, State = [state_root | _], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_number, [V, $-]} | State], Acc, Config);
tokenize(<<$+, V, T/binary>>, State = [state_root | _], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_number, [V, $+]} | State], Acc, Config);
tokenize(<<V, T/binary>>, State = [state_root | _], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_number, [V]} | State], Acc, Config);

tokenize(<<V, T/binary>>, [{state_number, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_number, [V | Num]} | Tail], Acc, Config);
tokenize(<<$., V, T/binary>>, [{state_number, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_decimal, [V, $. | Num]} | Tail], Acc, Config);
tokenize(<<$e, $+, V, T/binary>>, [{state_number, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $+, $e, $0, $. | Num]} | Tail], Acc, Config);
tokenize(<<$e, $-, V, T/binary>>, [{state_number, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $-, $e, $0, $. | Num]} | Tail], Acc, Config);
tokenize(<<$e, V, T/binary>>, [{state_number, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $+, $e, $0, $. | Num]} | Tail], Acc, Config);
tokenize(<<$E, $+, V, T/binary>>, [{state_number, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $+, $E, $0, $. | Num]} | Tail], Acc, Config);
tokenize(<<$E, $-, V, T/binary>>, [{state_number, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $-, $E, $0, $. | Num]} | Tail], Acc, Config);
tokenize(<<$E, V, T/binary>>, [{state_number, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $+, $E, $0, $. | Num]} | Tail], Acc, Config);
tokenize(T, [{state_number, Num} | Tail], Acc, Config) ->
    tokenize(T, Tail, [{number, list_to_integer(lists:reverse(Num))} | Acc], Config);

tokenize(<<V, T/binary>>, [{state_decimal, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_decimal, [V | Num]} | Tail], Acc, Config);
tokenize(<<$e, $+, V, T/binary>>, [{state_decimal, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $+, $e | Num]} | Tail], Acc, Config);
tokenize(<<$e, $-, V, T/binary>>, [{state_decimal, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $-, $e | Num]} | Tail], Acc, Config);
tokenize(<<$e, V, T/binary>>, [{state_decimal, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $+, $e | Num]} | Tail], Acc, Config);
tokenize(<<$E, $+, V, T/binary>>, [{state_decimal, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $+, $E | Num]} | Tail], Acc, Config);
tokenize(<<$E, $-, V, T/binary>>, [{state_decimal, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $-, $E | Num]} | Tail], Acc, Config);
tokenize(<<$E, V, T/binary>>, [{state_decimal, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $+, $E | Num]} | Tail], Acc, Config);
tokenize(T, [{state_decimal, Num} | Tail], Acc, Config) ->
    tokenize(T, Tail, [{number, list_to_float(lists:reverse(Num))} | Acc], Config);

tokenize(<<V, T/binary>>, [{state_frac, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V | Num]} | Tail], Acc, Config);
tokenize(T, [{state_frac, Num} | Tail], Acc, Config) ->
    tokenize(T, Tail, [{number, list_to_float(lists:reverse(Num))} | Acc], Config);

tokenize(<<$", T/binary>>, [{state_string, String}, {state_pattern} | Tail], Acc, Config) ->
    tokenize(T, Tail, [{regex, unicode:characters_to_binary(lists:reverse(String))} | Acc], Config);
tokenize(<<$", T/binary>>, [{state_string, String} | Tail], Acc, Config) ->
    tokenize(T, Tail, [{string, unicode:characters_to_binary(lists:reverse(String))} | Acc], Config);
tokenize(<<$\\, T/binary>>, [{state_string, String}, {state_pattern} | Tail], Acc, Config) -> % do not apply escape sequence in regex
    tokenize(T, [{state_string, [$\\ | String]}, {state_pattern} | Tail], Acc, Config);
tokenize(<<$\\, $n, T/binary>>, [{state_string, String} | Tail], Acc, Config=#config{string_apply_escape_sequence = true}) ->
    tokenize(T, [{state_string, [$\n | String]} | Tail], Acc, Config);
tokenize(<<$\\, $r, T/binary>>, [{state_string, String} | Tail], Acc, Config=#config{string_apply_escape_sequence = true}) ->
    tokenize(T, [{state_string, [$\r | String]} | Tail], Acc, Config);
tokenize(<<$\\, $t, T/binary>>, [{state_string, String} | Tail], Acc, Config=#config{string_apply_escape_sequence = true}) ->
    tokenize(T, [{state_string, [$\t | String]} | Tail], Acc, Config);
tokenize(<<$\\, $b, T/binary>>, [{state_string, String} | Tail], Acc, Config=#config{string_apply_escape_sequence = true}) ->
    tokenize(T, [{state_string, [$\b | String]} | Tail], Acc, Config);
tokenize(<<$\\, $f, T/binary>>, [{state_string, String} | Tail], Acc, Config=#config{string_apply_escape_sequence = true}) ->
    tokenize(T, [{state_string, [$\f | String]} | Tail], Acc, Config);
tokenize(<<$\\, $s, T/binary>>, [{state_string, String} | Tail], Acc, Config=#config{string_apply_escape_sequence = true}) ->
    tokenize(T, [{state_string, [$\s | String]} | Tail], Acc, Config);
tokenize(<<$\\, $", T/binary>>, [{state_string, String} | Tail], Acc, Config=#config{string_apply_escape_sequence = true}) ->
    tokenize(T, [{state_string, [$" | String]} | Tail], Acc, Config);
tokenize(<<$\\, $\\, T/binary>>, [{state_string, String} | Tail], Acc, Config=#config{string_apply_escape_sequence = true}) ->
    tokenize(T, [{state_string, [$\\ | String]} | Tail], Acc, Config);
tokenize(<<$\\, $u, A, B, C, D, T/binary>>, [{state_string, String} | Tail], Acc, Config=#config{string_apply_escape_sequence = true})
  when ?is_hexa(A), ?is_hexa(B), ?is_hexa(C), ?is_hexa(D) ->
    CodePoint = list_to_integer([A, B, C, D], 16), % CodePoint is *NOT* tested nor filtered.
    tokenize(T, [{state_string, [CodePoint | String]} |  Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#20, C < 16#D800 ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C > 16#DFFF, C < 16#FDD0 ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C > 16#FDEF, C < 16#FFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#10000, C < 16#1FFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#20000, C < 16#2FFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#30000, C < 16#3FFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#40000, C < 16#4FFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#50000, C < 16#5FFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#60000, C < 16#6FFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#70000, C < 16#7FFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#80000, C < 16#8FFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#90000, C < 16#9FFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#A0000, C < 16#AFFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#B0000, C < 16#BFFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#C0000, C < 16#CFFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#D0000, C < 16#DFFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#E0000, C < 16#EFFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#F0000, C < 16#FFFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config);
tokenize(<<C/utf8, T/binary>>, [{state_string, String} | Tail], Acc, Config) when C >= 16#100000, C < 16#10FFFE ->
    tokenize(T, [{state_string, [C | String]} | Tail], Acc, Config).
