ejpet
=====
Matching JSON nodes in Erlang.

What for ?
=====

Find if a JSON document has some structural properties, and possibly extract some information.

Kind of regular expression applied to JSON documents.

Useful to extract small data pieces from large JSON documents.


Example
=====

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

