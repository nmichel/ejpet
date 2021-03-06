ejpet
=====
Matching JSON nodes in Erlang.

[![hex.pm version](https://img.shields.io/hexpm/v/ejpet.svg)](https://hex.pm/packages/ejpet)
[![Build Status](https://travis-ci.org/nmichel/ejpet.png)](https://travis-ci.org/nmichel/ejpet)

What for ?
=====

Kind of regular expression applied to JSON documents.

* Find if a JSON document has some structural properties, and possibly extract some information.
* Useful to extract small data pieces from large JSON documents.
* Efficient filtering of JSON nodes in real time.

Backends for jsone, jsx, jiffy and mochijson2.


Quick start
=====

Obtain ```ejpet```
-----

#### Add it to your project

Add a dependency to ```ejpet``` and possibly to a supported JSON
codec in your project dependency set.

* With ```rebar3```, in ```rebar.config``` file

```erlang
{deps, [
    %% ...
    {ejpet, ".*", {git, "git://github.com/nmichel/ejpet.git", {tag, "0.7.0"}},
    {jsx, ".*", {git, "https://github.com/talentdeficit/jsx.git", {tag, "v2.8.3"}},
    %% ...
]}.
```

* With ```mix```, in ```mix.exs``` file

```elixir
defmodule MyProject.Mixfile do
  use Mix.Project
  
  def project do
    [
      # ...
      deps: deps()
      # ...
    ]
  end
  
  defp deps() do
    [
      # ...
      {:ejpet, "~> 0.7.0"},
      {:jsx, "~> 2.8"},
      # ...
    ]
  end
end
```

#### From source

Clone

```shell
$ git clone git@github.com:nmichel/ejpet.git
```

Build

```shell
$ cd ejpet
$ ./rebar get-deps
$ make && make test
```

Start Erlang shell

```shell
erl -pz ./ebin ./deps/*/ebin
```

Start (m)using 
----

Read some JSON data

```erlang
1> {ok, Data} = file:read_file("./test/channels_list.json").
{ok,<<239,187,191,91,13,10,32,32,32,32,123,13,10,32,32,
      32,32,32,32,32,32,34,110,117,109,98,101,...>>}
```

Decode JSON using, say, jsx (provided you have ```jsx``` in your load path)

```erlang
2> Node = jsx:decode(Data).
[[{<<"number">>,1},
  {<<"lcn">>,2},
  {<<"name">>,<<"France 2">>},
  {<<"sap_group">>,<<>>},
  {<<"ip_multicast">>,<<"239.100.10.1">>},
  {<<"port_multicast">>,1234},
  {<<"num_clients">>,0},
  {<<"scrambling_ratio">>,0},
  {<<"is_up">>,1},
  {<<"pcr_pid">>,120},
  {<<"pmt_version">>,4},
  {<<"unicast_port">>,0},
  {<<"service_id">>,257},
  {<<"service_type">>,
   <<"Please report : Unknown service type doc : EN 30"...>>},
  {<<"pids_num">>,7},
  {<<"pids">>,
...
```

Ok. Now define what we are looking for, and what we want to get

    Find somewhere in a list, an object with
    * a {"ip_multicast", "239.100.10.4"} pair
    * a key "pcr_pid", whatever value captured in variable "pcr",
    * a key "pids", which value is either a list or an object into which there are
      * an object with
        * a key "language" which value matches regex "^fr",
        * a key "number", whatever value captured in variable "apid"
        * a key "type", whatever value captured in variable "acodec"
      * an object with
        * a key "type", which value matches regex "Video" captured in variable "vcodec"
        * a key "number", whatever value captured in variable "vpid"

```erlang
3>  O = ejpet:compile("[*, {\"ip_multicast\":\"239.100.10.4\",
                            \"pcr_pid\":(?<pcr>_),
                            \"pids\":<{\"language\": #\"^fr\",
                                       \"number\": (?<apid>_),
                                       \"type\": (?<acodec>_)},
                                      {\"type\": (?<vcodec>#\"Video\"),
                                       \"number\": (?<vpid>_)}>}, *]", jsx).
{ejpet,jsx,#Fun<ejpet_jsx_generators.9.11467207>}
```


Run and seek ...

```erlang
4>  ejpet:run(Node, O).
```

Here you are !

    {true,[{"vpid",520},
           {"vcodec",[<<"Video (MPEG2)">>]},
           {"acodec",[<<"Audio (MPEG1)">>]},
           {"apid",530},
           {"pcr",520}]}


How ?
=====

Express what you want to match using a simple expression language.


Expression syntax
----

| pattern | match ? | Notes |
 ------------------|------------|------------------------------
| `true` | `true` | |
| `false` | `false` | |
| `null` | `null` | |
| `"string"` | the string `"string"` | UTF-8 encoded string (with escaping) | 
| `#"regex"` | any string matching regex `"regex"` | UTF-8 encoded string (no escaping) |
| `number` | the number `number` e.g. (`42`, `3.14159`, `-3395.1264e-22` ) | |
| `{ kv* }` | object for which all kv (key/value) patterns are matched | Order does not matter |
| `[ item* (, *)?]` | list for which all item patterns are matched | Order DOES matter |
| `< value* >` | value set (list, or object values) for which all value patterns are matched | Order does not matter 
| `< value* >/g` | same as previous but search for ALL matches. Useful only when capturing | Order does not matter
| `<! value* !>` | same as `< value* >` but search deep. |
| `<! value* !>/g` | same as previous but search for ALL matches. Useful only when capturing |  
| `(?<name>expr)` | capture expression `expr` in return value `name` | Every JSON expression may be captured
| `(!<name>type)` | match json object of type `type` against parameter named `name`  |


`kv` may be one of the form
* _:pattern
* `"key"`:`_`
* `"key"`:pattern

`item` may be one of the form
* `*, ` pattern
* pattern

`value` is a pattern

`kv`, `item` and `value` are separated by `,`.


In parameter injection `type`may be
* `number`
* `boolean`
* `string`
* `regex`

## Notes

### Numbers

`number` matching may be strict or loose, depending on an option passed are compile-time.

```erlang
1> ejpet:match(<<"42.0">>, "42").
{true,<<"{}">>}
2> ejpet:match(<<"42.0">>, "42", [{number_strict_match, true}]).
{false,<<"{}">>}
```

### Strings and Regex

`string` and `regex` are UTF-8 encoded byte streams.

They may contain escaping sequences, as in `"\\b"`, or `"\u00E9"`. When found in a `string` these sequences are interpreted by default (but they may be left as-is with option `string_apply_escape_sequence` set to `false`). Found in `regex` they are not interpreted.

```erlang
3> ejpet:match(<<"\"\x{00E9}\""/utf8>>, <<"\"\\u00E9\""/utf8>>, [{string_apply_escape_sequence, true}]).
{true,<<"{}">>}
4> ejpet:match(<<"\"\x{00E9}\""/utf8>>, <<"\"\\u00E9\""/utf8>>, [{string_apply_escape_sequence, false}]).
{false,<<"{}">>}
5> ejpet:match(<<"\"\\\\u00E9\""/utf8>>, <<"\"\\u00E9\""/utf8>>, [{string_apply_escape_sequence, false}]).
{true,<<"{}">>}
```

Codepoint produced by evaluating an escape sequence of the form `\uABCD` is *NOT* checked. One can insert any codepoint, valid or not, in a string or regex.

Captures
----

Every pattern `p` can be captured by simply substituing it by `(?<variable_name>p)`. Captures are returned as a JSON object, where each `variable_name` ìs a key, and the list if captures found for that variable is the value.

This JSON object is build with repect to the backend indicated when compiling the pattern. 

**Warning** : if there is no captures to return, the empty JSON object `{}` will be returned. But its actual form depends on the backend.

* jsx: `[{}]`
* jiffy: `{[]}`
* mochijson: `{struct, []}`
* jsone: `#{}`

One may wonder why return captures as a encoded JSON object. There is 2 reasons :
  1. captures objects are captured "as is" in the parsed document, i.e. in their encoded form. Using the backend encoding for the result is more coherent;
  2. capture JSON object can itself be pattern matched.

Parameters Injection
----

It is possible to provide some matching values at match-time, through parameter injection forms like `(!<param_name>param_type)`, where `param_type` may be `number`, `string`, `boolean` and `regex`.
At match-time, produced matching functions will look for an entry named `param_name` in the provided parameters list. See `ejpet:run/3` and `ejpet:match/4`.


Note that `string` values should be binaries, and `regex` values MUST be `mp()` opaque objects returned by `re:compile/2`.


# API

```erlang
backend() = jsx | jiffy | mochijson2 | jsone
epm() = {ejpet, term(), term()}
expr_src() = string()
compile_option() = {string_apply_escape_sequence, boolean()}
                 | {number_strict_match, boolean()}

json_input() = string() | binary()
json_src() = binary()
json_term() = jsx_term() | jiffy_term() | mochijson2_term()

run_param_name = binary()
run_param_value = boolean() | number() | binary() | re::mp()
run_param = {run_param_name(), run_param_value()}                                                                                                                                                                  

run_res() = {match_stat(), json_term()}
match_res() = {match_stat(), json_src()}
match_stat() = true | false

ejpet:decode(JSONText, Backend) -> json_term()

  JSONText = json_input()
  Backend = backend()

ejpet:encode(JSONTerm, Backend) -> json_term()

  JSONTerm = json_term()
  Backend = backend()

ejpet:compile(Expr, Backend, Options) -> epm()

  Expr = expr_src()
  Backend = backend()
  Options = [Option]
  Option = compile_option()

ejpet:compile(Expr, Backend) -> epm()

  Same as ejpet:compile(Expr, Backend, [])
  
ejpet:compile(Expr) -> epm()

  Same as ejpet:compile(Expr, jsx, [])

ejpet:backend(EPM) -> backend()

  EPM = epm()

ejpet:run(JSONTerm, EPM, Params) -> run_res()

  EPM = epm()
  JSONTerm = json_term()
  Params = [Param]
  Param = run_param()

ejpet:run(JSONTerm, EPM) -> run_res()

  Same pas ejpet:run(JSONTerm, EPM, [])

ejpet:match(JSONText, Expr, Options, Params) -> match_res()

  JSONText = json_input()
  Expr = expr_src() | epm()
  Options = [Option]
  Option = compile_option()
  Params = [Param]
  Param = run_param()

ejpet:match(JSONText, Expr, Options) -> match_res()

  Same as ejpet:match(JSONText, Expr, Options, [])
  
ejpet:match(JSONText, Expr) -> match_res()

  Same as ejpet:match(JSONText, Expr, [], [])
  
ejpet:get_status(Res) -> match_stat()

  Res = run_res() | match_res()

get_captures(Res) -> json_term()

  Res = run_res() | match_res()
  
get_capture(Res, Name) -> {ok, json_term()} | not_found

  Same as get_captures(Res, Name, jsx)

get_capture(Res, Name, Backend) ->  {ok, json_term()} | not_found

  Res = run_res()
  Name = string() | binary()
  Backend = backend()

empty_capture_set() -> json_term()

  Same as empty_capture_set(jsx)
  
empty_capture_set(Backend) -> json_term()

  Backend = backend()
```

# Examples

## Basics


| Expression | Match | No match | Code snippet |
 ------------------|------------------------------------------|-------------------------------------|-----
| `42` | `42` | `"42"`, `[42]`, `{"key": 42}` | `ejpet:match(<<"42">>, "42").` |
| `"42"` | `"42"` | `42`, `["42"]`, `{"key": "42"}` | `ejpet:match(<<"\"42\"">>, "\"42\"").` |
| `true` | `true` | `"true"`, `[true]` | `ejpet:match(<<"true">>, "true").` |
| `false` | `false` | `"false"`, `[false]` | `ejpet:match(<<"false">>, "false").` |
| `null` | `null` | `"null"`, `[null]` | `ejpet:match(<<"null">>, "null").` |
| `#"foo"` | `"foobar"`,  `"barfoo"` |  `"barfo"` | `ejpet:match(<<"\"foobar\"">>, "#\"foo\"").` |
| `#"^foo"` | `"foobar"` | `"barfoo"` | `ejpet:match(<<"\"foobar\"">>, "#\"^foo\"").` |
| `#"bar$"` | `"foobar"` | `"barfoo"` | `ejpet:match(<<"\"foobar\"">>, "#\"bar$\"").` |

## Objects


| Expression | Match | No match | Code snippet |
 ------------------|------------------------------------------|-------------------------------------|-----
| `{_:42}` | `{"bar": 42}`, `{"bar": 47, "foo": 42}` | `{"bar": 47}`, `{"foo": "42"}` | `ejpet:match(<<"{\"foo\": 42}">>, "{_:42}").` |
| `{"foo":_}` | `{"foo": 42}`, `{"bar": 42, "foo": {}}` | `{"bar": "foo"}` | `ejpet:match(<<"{\"foo\": 42}">>, "{\"foo\":_}").` |
| `{"foo":42}` | `{"foo": 42}`, `{"bar": "42", "foo": 42}` | `{"bar": 42, "foo": "42"}` | `ejpet:match(<<"{\"foo\": 42}">>, "{\"foo\":42}").`|
| `{_:{"foo": 42}, "bar": {_:#"bar"}}` | `{"neh": {"foo": 42}, "bar": {"nimp": "foobar"}}` | `{"neh": {"notfoo": 42}, "bar": {"nimp": "foobar"}}` | `ejpet:match(<<"{\"neh\": {\"foo\": 42}, \"bar\": {\"nimp\": \"foobar\"}}">>, "{_:{\"foo\": 42}, \"bar\": {_:#\"bar\"}}").`|

## Lists

| Expression | Match | No match | Code snippet |
 ------------------|------------------------------------------|-------------------------------------|-----
| `["42"]` | `["42"]` | `{"bar": "42"}`, `{"foo": 42}`, `[42]`, `["42", "42"]` | `ejpet:match(<<"[\"42\"]">>, "[\"42\"]").` |
| `[*, "42"]` | `["42"]`, `["42", "42"]`, `[true, "42"]` | `{"bar": "42"}`, `{"foo": 42}`, `[42]`, `["42", true]` | `ejpet:match(<<"[true, \"42\"]">>, "[*, \"42\"]").` |
| `[*, "42", *]` | `["42"]`, `["42", "42"]`, `[true, "42"]`, `["42", true]`, `[{}, "42", true]` | `{"bar": "42"}`, `{"foo": 42}`, `[42]` | `ejpet:match(<<"[true, \"42\", {}]">>, "[*, \"42\", *]").` |
| `[[42]]` | `[[42]]` | `[42]`, `[[42], 42]` | `ejpet:match(<<"[[42]]">>, "[[42]]").` |
| `[*, [42]]` | `[[42]]`, `["42", [42]]` | `[[42], 42]` | `ejpet:match(<<"[\"42\", [42]]">>, "[*, [42]]").` |
| `[[42], *]` | `[[42]]`, `[[42], 42]` | `["42", [42]]` | `ejpet:match(<<"[[42], \"42\"]">>, "[[42], *]").` |

## Value sets (lists or object value set)

| Expression | Match | No match | Code snippet |
 ------------------|------------------------------------------|-------------------------------------|-----
| `<42>` | `[42]`, `{"key": 42}` | `42`, `"42"` | `ejpet:match(<<"{\"key\": 42}">>, "<42>").` |
| `<"42">` | `["42"]`, `{"bar": "42"}`, `[42, "42"]`, `["42", 42]` | `[42]`, `{"bar": 47}`, `{"foo": 42}` | `ejpet:match(<<"{\"bar\": \"42\"}">>, "<\"42\">").` |
| `<!"42"!>` | `["42"]`, `[true, "42"]`, `["foo", ["42", true], {}]`, `[{}, {"foo": "42"}, true]`, `{"bar": "42"}`, `{"bar": {"foo": "42"}}` | `"42"`, `{"foo": 42}`, `[42]` | `ejpet:match(<<"[true, [null, {\"foo\": \"42\"}, \"bar\"], {}]">>, "<!\"42\"!>").` |
| `<!<!"42"!>!>` | `[["42"]]`, `[{}, {"foo": "42"}, true]`, `{"bar": {"foo": "42"}}` | `["42"]`, `{"bar": "42"}` | `ejpet:match(<<"[{\"foo\":\"42\"}]">>, "<!<!\"42\"!>!>").` |

## Captures

| Expression | Test | Capture(s) | Code snippet |
---|---|---|----
| `<!(?<subnode>{_:42})!>` | `[{"foo": null}, {"foo": 42, "bar": {}}]` | subnode: `[{"foo":42,"bar":{}}]` | `ejpet:match(<<"[{\"foo\": null}, {\"foo\": 42, \"bar\": {}}]">>, "<!(?<subnode>{_:42})!>").`
| `(?<all><!(?<subnode>{_:42})!>)` | `[{"foo": null}, {"foo": 42, "bar": {}}]` | all: `[[{"foo":null},{"foo":42,"bar":{}}]]`,subnode: `[{"foo":42,"bar":{}}]` | `ejpet:match(<<"[{\"foo\": null}, {\"foo\": 42, \"bar\": {}}]">>, "(?<all><!(?<subnode>{_:42})!>)").`

## Global captures

| Expression | Test | Capture(s) | Code snippet |
---|---|---|----
|`<(?<node>{\"codec\":_, \"lang\":(?<lang>_)})>/g`|`[{"codec": "audio", "lang": "fr"}, {"codec": "video", "lang": "en"}, {"codec": "foo", "lang": "it"}]`|node: `[{"codec":"audio","lang":"fr"}, {"codec":"video","lang":"en"}, {"codec":"foo","lang":"it"}]` lang: `["fr", "en", "it"]`| `ejpet:match(<<"[{\"codec\": \"audio\", \"lang\": \"fr\"}, {\"codec\":\"video\", \"lang\": \"en\"}, {\"codec\": \"foo\", \"lang\": \"it\"}]">>, <<"<(?<node>{\"codec\":_, \"lang\":(?<lang>_)})>/g">>)`

## Injections

| Expression | Test | parameters | Capture(s) | Code snippet |
---|---|---|---|---
| `<(?<subnode>(!<what>number))>` | `[41, 42, 43]` | `[{<<"what">>, 42}]` | subnode: `[42]` | `ejpet:match(<<"[41, 42, 43]">>, "<(?<subnode>(!<what>number))>", [], [{<<"what">>, 42}]).`

# Notes

In arrays above, captured values are expressed as "abstract JSON node", for illustration purpose.
As explained previously, actual capture result depends on the API function used, and may be:

* serialized JSON nodes (as in the "Code snippet" column), with `ejpet:match()`

```erlang
1> ejpet:match(<<"[{\"foo\": null}, {\"foo\": 42, \"bar\": {}}]">>, "(?<all><!(?<subnode>{_:42})!>)").
{true,<<"{\"all\":[[{\"foo\":null},{\"foo\":42,\"bar\":{}}]],\"subnode\":[{\"foo\":42,\"bar\":{}}]}">>}
```

* (jsx | jiffy | mochijson2) JSON value, depending on the backend, for easier further processing, with `ejpet:run()`

```erlang
1> JSX = ejpet:compile("(?<all><!(?<subnode>{_:42})!>)", jsx, []).
{ejpet,jsx,#Fun<ejpet_jsx_generators.19.98422695>}
2> ejpet:run((ejpet:backend(JSX)):decode(<<"[{\"foo\": null}, {\"foo\": 42, \"bar\": {}}]">>), JSX).
{true,[{"all",
        [[[{<<"foo">>,null}],[{<<"foo">>,42},{<<"bar">>,[{}]}]]]},
       {"subnode",[[{<<"foo">>,42},{<<"bar">>,[{}]}]]}]}

39> Mochi = ejpet:compile("(?<all><!(?<subnode>{_:42})!>)", mochijson2, []).
{ejpet,mochijson2,
       #Fun<ejpet_mochijson2_generators.19.110863078>}
40> ejpet:run((ejpet:backend(Mochi)):decode(<<"[{\"foo\": null}, {\"foo\": 42, \"bar\": {}}]">>), Mochi).
{true,{struct,[{<<"all">>,
                [[{struct,[{<<"foo">>,null}]},
                  {struct,[{<<"foo">>,42},{<<"bar">>,{struct,[]}}]}]]},
               {<<"subnode">>,
                [{struct,[{<<"foo">>,42},{<<"bar">>,{struct,[]}}]}]}]}}
```

