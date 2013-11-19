%% -*- coding: utf-8 -*-

-module(ejpet_tests).
-author('nicolas.michel.lava@gmail.com').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(BACKEND, jsx).
-define(REF_BACKEND, jsx).

run_test_() ->
    Tests = [{"**/42",
              <<"42"/utf8>>,
              {false, []}},
             {"(?<full>**/42)",
              <<"[42]"/utf8>>,
              {true, [{"full", <<"[42]">>}]}},
             {"**/(?<toto>42)",
              <<"[[[[[[[[[[[[[[[[[[[[[[[[{\"foo\": 42}]]]]]]]]]]]]]]]]]]]]]]]]"/utf8>>,
              {true, [{"toto", <<"42">>}]}},
              {"**/(?<toto><42>)",
               <<"[[[[[[[[[[[[[[[[[[[[[[[[{\"foo\": 42}]]]]]]]]]]]]]]]]]]]]]]]]"/utf8>>,
               {true,[{"toto", <<"{\"foo\":42}">>}]}},
              {"**/(?<toto>[42])",
               <<"[[[[[[[[[[[[[[[[[[[[[[[[1, 42]]]]]]]]]]]]]]]]]]]]]]]]"/utf8>>,
               {false, []}},
             {"[*, {\"ip_multicast\":(?<ip>_)}, *]",
              <<"[
    {
        \"number\": 1,
        \"lcn\": 2,
        \"name\": \"France 2\",
        \"sap_group\": \"\",
        \"ip_multicast\": \"239.100.10.1\",
        \"port_multicast\": 1234,
        \"num_clients\": 0,
        \"scrambling_ratio\": 0,
        \"is_up\": 1,
        \"pcr_pid\": 120,
        \"pmt_version\": 4,
        \"unicast_port\": 0,
        \"service_id\": 257,
        \"service_type\": \"Please report : Unknown service type doc : EN 300 468 v1.13.1 table 87\",
        \"pids_num\": 7,
        \"pids\": [
            {
                \"number\": 110,
                \"type\": \"PMT\",
                \"language\": \"---\"
            },
            {
                \"number\": 120,
                \"type\": \"Video (MPEG2)\",
                \"language\": \"---\"
            },
            {
                \"number\": 130,
                \"type\": \"Audio (MPEG1)\",
                \"language\": \"fra\"
            },
            {
                \"number\": 131,
                \"type\": \"Audio (MPEG1)\",
                \"language\": \"qad\"
            },
            {
                \"number\": 132,
                \"type\": \"Audio (MPEG1)\",
                \"language\": \"qaa\"
            },
            {
                \"number\": 140,
                \"type\": \"Subtitling\",
                \"language\": \"fra\"
            },
            {
                \"number\": 141,
                \"type\": \"Subtitling\",
                \"language\": \"fra\"
            }
        ]
    },
    {
        \"number\": 2,
        \"lcn\": 3,
        \"name\": \"France 3\",
        \"sap_group\": \"\",
        \"ip_multicast\": \"239.100.10.2\",
        \"port_multicast\": 1234,
        \"num_clients\": 0,
        \"scrambling_ratio\": 0,
        \"is_up\": 1,
        \"pcr_pid\": 220,
        \"pmt_version\": 6,
        \"unicast_port\": 0,
        \"service_id\": 273,
        \"service_type\": \"Please report : Unknown service type doc : EN 300 468 v1.13.1 table 87\",
        \"pids_num\": 5,
        \"pids\": [
            {
                \"number\": 210,
                \"type\": \"PMT\",
                \"language\": \"---\"
            },
            {
                \"number\": 220,
                \"type\": \"Video (MPEG2)\",
                \"language\": \"---\"
            },
            {
                \"number\": 230,
                \"type\": \"Audio (MPEG1)\",
                \"language\": \"fra\"
            },
            {
                \"number\": 231,
                \"type\": \"Audio (MPEG1)\",
                \"language\": \"qad\"
            },
            {
                \"number\": 240,
                \"type\": \"Subtitling\",
                \"language\": \"fra\"
            }
        ]
    },
    {
        \"number\": 3,
        \"lcn\": 5,
        \"name\": \"France 5\",
        \"sap_group\": \"\",
        \"ip_multicast\": \"239.100.10.3\",
        \"port_multicast\": 1234,
        \"num_clients\": 0,
        \"scrambling_ratio\": 0,
        \"is_up\": 1,
        \"pcr_pid\": 320,
        \"pmt_version\": 3,
        \"unicast_port\": 0,
        \"service_id\": 260,
        \"service_type\": \"Please report : Unknown service type doc : EN 300 468 v1.13.1 table 87\",
        \"pids_num\": 5,
        \"pids\": [
            {
                \"number\": 310,
                \"type\": \"PMT\",
                \"language\": \"---\"
            },
            {
                \"number\": 320,
                \"type\": \"Video (MPEG2)\",
                \"language\": \"---\"
            },
            {
                \"number\": 330,
                \"type\": \"Audio (MPEG1)\",
                \"language\": \"fra\"
            },
            {
                \"number\": 331,
                \"type\": \"Audio (MPEG1)\",
                \"language\": \"qad\"
            },
            {
                \"number\": 340,
                \"type\": \"Subtitling\",
                \"language\": \"fra\"
            }
        ]
    },
    {
        \"number\": 4,
        \"lcn\": 19,
        \"name\": \"France Ô\",
        \"sap_group\": \"\",
        \"ip_multicast\": \"239.100.10.4\",
        \"port_multicast\": 1234,
        \"num_clients\": 0,
        \"scrambling_ratio\": 0,
        \"is_up\": 1,
        \"pcr_pid\": 520,
        \"pmt_version\": 3,
        \"unicast_port\": 0,
        \"service_id\": 261,
        \"service_type\": \"Please report : Unknown service type doc : EN 300 468 v1.13.1 table 87\",
        \"pids_num\": 4,
        \"pids\": [
            {
                \"number\": 510,
                \"type\": \"PMT\",
                \"language\": \"---\"
            },
            {
                \"number\": 520,
                \"type\": \"Video (MPEG2)\",
                \"language\": \"---\"
            },
            {
                \"number\": 530,
                \"type\": \"Audio (MPEG1)\",
                \"language\": \"fra\"
            },
            {
                \"number\": 540,
                \"type\": \"Subtitling\",
                \"language\": \"fra\"
            }
        ]
    },
    {
        \"number\": 5,
        \"lcn\": 13,
        \"name\": \"LCP\",
        \"sap_group\": \"\",
        \"ip_multicast\": \"239.100.10.5\",
        \"port_multicast\": 1234,
        \"num_clients\": 0,
        \"scrambling_ratio\": 0,
        \"is_up\": 1,
        \"pcr_pid\": 620,
        \"pmt_version\": 2,
        \"unicast_port\": 0,
        \"service_id\": 262,
        \"service_type\": \"Please report : Unknown service type doc : EN 300 468 v1.13.1 table 87\",
        \"pids_num\": 4,
        \"pids\": [
            {
                \"number\": 610,
                \"type\": \"PMT\",
                \"language\": \"---\"
            },
            {
                \"number\": 620,
                \"type\": \"Video (MPEG2)\",
                \"language\": \"---\"
            },
            {
                \"number\": 630,
                \"type\": \"Audio (MPEG1)\",
                \"language\": \"fra\"
            },
            {
                \"number\": 640,
                \"type\": \"Subtitling\",
                \"language\": \"fra\"
            }
        ]
    }
]"/utf8>>,
              {true, [{"ip", <<"\"239.100.10.1\"">>}]}}
             ],

    lists:foldl(fun({Pattern, JSON, Expected}, Acc) ->
                        %% Execute the test, using the specified backend
                        %% 
                        O = ejpet:compile(Pattern, ?BACKEND),
                        {Status, Captures} = ejpet:run(O, (ejpet:backend(O)):decode(JSON)),

                        %% Transform captures to text
                        %% 
                        JSONCaptures = [{VarName, (ejpet:backend(O)):encode(Cap)} || {VarName, Cap} <- Captures],

                        %% Parse again and stringify captures using the reference backend
                        %% 
                        RefCaptures = [{VarName, ?REF_BACKEND:encode(?REF_BACKEND:decode(Cap))} || {VarName, Cap} <- JSONCaptures],

                        %% Produce test function
                        %% 
                        [?_test(?assert(Expected == {Status, RefCaptures})) | Acc]
                end, [], Tests).

mumu_test_deactivate() ->
    {ok, Data} = file:read_file("./test/channels_list.json"),
    Node = jsx:decode(Data),
    O = ejpet:compile("[*, {\"ip_multicast\":\"239.100.10.4\",
                            \"pcr_pid\":(?<pcr>_),
                            \"pids\":<{\"language\": #\"^fr\",
                                       \"number\": (?<apid>_),
                                       \"type\": (?<acodec>_)},
                                      {\"type\": (?<vcodec>#\"Video\"),
                                       \"number\": (?<vpid>_)}>}, *]", jsx),
    
    ejpet:run(O, Node).

-endif.
