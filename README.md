ejpet
=====
Matching JSON nodes in Erlang.

What for ?
=====

Kind of regular expression applied to JSON documents.

* Find if a JSON document has some structural properties, and possibly extract some information.
* Useful to extract small data pieces from large JSON documents.
* Efficient filtering of JSON nodes in real time.

Backends for  jsx, jiffy and mochijson2.


Quick start
=====

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

Read some JSON data

```erlang
1> {ok, Data} = file:read_file("./test/channels_list.json").
{ok,<<239,187,191,91,13,10,32,32,32,32,123,13,10,32,32,
      32,32,32,32,32,32,34,110,117,109,98,101,...>>}
```

Decode JSON using, say, jsx

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
3>                          \"pcr_pid\":(?<pcr>_),
3>                          \"pids\":<{\"language\": #\"^fr\",
3>                                     \"number\": (?<apid>_),
3>                                     \"type\": (?<acodec>_)},
3>                                    {\"type\": (?<vcodec>#\"Video\"),
3>                                     \"number\": (?<vpid>_)}>}, *]", jsx).
{ejpet,jsx,#Fun<ejpet_jsx_generators.9.11467207>}
```


Run and seek ...

```erlang
4>  ejpet:run(O, Node).
```

Here you are !

    {true,[{"vpid",520},
           {"vcodec",<<"Video (MPEG2)">>},
           {"acodec",<<"Audio (MPEG1)">>},
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
| `"string"` | the string `"string"` | restricted to [a-zA-Z0-9$^ _.]+ | 
| `#"regex"` | any string matching regex `"regex"` | restricted to [a-zA-Z0-9$^ _.]+ |
| `number` | the number `number` e.g. (`42`) | restricted to [0-9]+ |
| `{ kv* }` | object for which all kv (key/value) patterns are matched | Order does not matter |
| `[ item* (, *)?]` | list for which all item patterns are matched | Order DOES matter |
| `< value* >` | value set (list, or object values) for which all value patterns are matched | Order does not matter |

`kv` may be one of the form
* _:pattern
* `"key"`:`_`
* `"key"`:pattern

`item` may be one of the form
* `*, ` pattern
* pattern

`value` is a pattern

`kv`, `item` and `value` are separated by `,`.

Captures
----

Every pattern `p` can be captured by simply substituing it by `(?<variable_name>p)`.

# Examples

## Basics


| Expression | Match | No match | Code snippet |
 ------------------|------------------------------------------|-------------------------------------|-----
| `42` | `42` | `"42"`, `[42]`, `{"key": 42}` | `ejpet:match("42", <<"42">>).` |
| `"42"` | `"42"` | `42`, `["42"]`, `{"key": "42"}` | `ejpet:match("\"42\"", <<"\"42\"">>).` |
| `true` | `true` | `"true"`, `[true]` | `ejpet:match("true", <<"true">>).` |
| `false` | `false` | `"false"`, `[false]` | `ejpet:match("false", <<"false">>).` |
| `null` | `null` | `"null"`, `[null]` | `ejpet:match("null", <<"null">>).` |
| `#"foo"` | `"foobar"`,  `"barfoo"` |  `"barfo"` | `ejpet:match("#\"foo\"", <<"\"foobar\"">>).` |
| `#"^foo"` | `"foobar"` | `"barfoo"` | `ejpet:match("#\"^foo\"", <<"\"foobar\"">>).` |
| `#"bar$"` | `"foobar"` | `"barfoo"` | `ejpet:match("#\"bar$\"", <<"\"foobar\"">>).` |

## Objects


| Expression | Match | No match | Code snippet |
 ------------------|------------------------------------------|-------------------------------------|-----
| `{_:42}` | `{"bar": 42}`, `{"bar": 47, "foo": 42}` | `{"bar": 47}`, `{"foo": "42"}` | `ejpet:match("{_:42}", <<"{\"foo\": 42}">>).` |
| `{"foo":_}` | `{"foo": 42}`, `{"bar": 42, "foo": {}}` | `{"bar": "foo"}` | `ejpet:match("{\"foo\":_}", <<"{\"foo\": 42}">>).` |
| `{"foo":42}` | `{"foo": 42}`, `{"bar": "42", "foo": 42}` | `{"bar": 42, "foo": "42"}` | `ejpet:match("{\"foo\":42}", <<"{\"foo\": 42}">>).`|
| `{_:{"foo": 42}, "bar": {_:#"bar"}}` | `{"neh": {"foo": 42}, "bar": {"nimp": "foobar"}}` | `{"neh": {"notfoo": 42}, "bar": {"nimp": "foobar"}}` | `ejpet:match("{_:{\"foo\": 42}, \"bar\": {_:#\"bar\"}}", <<"{\"neh\": {\"foo\": 42}, \"bar\": {\"nimp\": \"foobar\"}}">>).`|

## Lists

| Expression | Match | No match | Code snippet |
 ------------------|------------------------------------------|-------------------------------------|-----
| `["42"]` | `["42"]` | `{"bar": "42"}`, `{"foo": 42}`, `[42]`, `["42", "42"]` | `ejpet:match("[\"42\"]", <<"[\"42\"]">>).` |
| `[*, "42"]` | `["42"]`, `["42", "42"]`, `[true, "42"]` | `{"bar": "42"}`, `{"foo": 42}`, `[42]`, `["42", true]` | `ejpet:match("[*, \"42\"]", <<"[true, \"42\"]">>).` |
| `[*, "42", *]` | `["42"]`, `["42", "42"]`, `[true, "42"]`, `["42", true]`, `[{}, "42", true]` | `{"bar": "42"}`, `{"foo": 42}`, `[42]` | `ejpet:match("[*, \"42\", *]", <<"[true, \"42\", {}]">>).` |
| `[[42]]` | `[[42]]` | `[42]`, `[[42], 42]` | `ejpet:match("[[42]]", <<"[[42]]">>).` |
| `[*, [42]]` | `[[42]]`, `["42", [42]]` | `[[42], 42]` | `ejpet:match("[*, [42]]", <<"[\"42\", [42]]">>).` |
| `[[42], *]` | `[[42]]`, `[[42], 42]` | `["42", [42]]` | `ejpet:match("[[42], *]", <<"[[42], \"42\"]">>).` |

## Value sets (lists or object value set)

| Expression | Match | No match | Code snippet |
 ------------------|------------------------------------------|-------------------------------------|-----
| `<42>` | `[42]`, `{"key": 42}` | `42`, `"42"` | `ejpet:match("<42>", <<"{\"key\": 42}">>).` |
| `<"42">` | `["42"]`, `{"bar": "42"}`, `[42, "42"]`, `["42", 42]` | `[42]`, `{"bar": 47}`, `{"foo": 42}` | `ejpet:match("<\"42\">", <<"{\"bar\": \"42\"}">>).` |
| `**/"42"` | `["42"]`, `[true, "42"]`, `["foo", ["42", true], {}]`, `[{}, {"foo": "42"}, true]`, `{"bar": "42"}`, `{"bar": {"foo": "42"}}` | `"42"`, `{"foo": 42}`, `[42]` | `ejpet:match("**/\"42\"", <<"[true, [null, {\"foo\": \"42\"}, \"bar\"], {}]">>).` |
| `**/**/"42"` | `[["42"]]`, `[{}, {"foo": "42"}, true]`, `{"bar": {"foo": "42"}}` | `["42"]`, `{"bar": "42"}` | `ejpet:match("**/**/\"42\"", <<"[{\"foo\":\"42\"}]">>).` |


    
Missing
=====

Currently only a small subset of characters is allowed in string and regexp, without utf8 support.

Numbers are limited to positive integers.
