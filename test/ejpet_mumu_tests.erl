%% -*- coding: utf-8 -*-

-module(ejpet_mumu_tests).
-author('nicolas.michel.lava@gmail.com').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


mumu_test_() ->
    MumuData =
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
    },
    {
        \"number\": 6,
        \"lcn\": 7,
        \"name\": \"ARTE\",
        \"sap_group\": \"\",
        \"ip_multicast\": \"239.100.13.1\",
        \"port_multicast\": 1234,
        \"num_clients\": 0,
        \"scrambling_ratio\": 0,
        \"is_up\": 1,
        \"pcr_pid\": 720,
        \"pmt_version\": 19,
        \"unicast_port\": 0,
        \"service_id\": 1543,
        \"service_type\": \"Please report : Unknown service type doc : EN 300 468 v1.13.1 table 87\",
        \"pids_num\": 9,
        \"pids\": [
            {
                \"number\": 700,
                \"type\": \"PMT\",
                \"language\": \"---\"
            },
            {
                \"number\": 720,
                \"type\": \"Video (MPEG2)\",
                \"language\": \"---\"
            },
            {
                \"number\": 730,
                \"type\": \"Audio (MPEG1)\",
                \"language\": \"fra\"
            },
            {
                \"number\": 731,
                \"type\": \"Audio (MPEG1)\",
                \"language\": \"deu\"
            },
            {
                \"number\": 732,
                \"type\": \"Audio (MPEG1)\",
                \"language\": \"eng\"
            },
            {
                \"number\": 733,
                \"type\": \"Audio (MPEG1)\",
                \"language\": \"qad\"
            },
            {
                \"number\": 740,
                \"type\": \"VBI Data\",
                \"language\": \"---\"
            },
            {
                \"number\": 750,
                \"type\": \"Subtitling\",
                \"language\": \"fra\"
            },
            {
                \"number\": 751,
                \"type\": \"Subtitling\",
                \"language\": \"fra\"
            }
        ]
    }]"/utf8>>,
    Tests = [
             {<<"[*, {\"name\": #\"ARTE\", \"pids\": <{\"type\": #\"Audio\", \"number\": (?<number>_), \"language\": (?<lang>_)}/g>}, *]">>,
              [{MumuData,
                {true,[{"number",[<<"730">>,<<"731">>,<<"732">>,<<"733">>]},
                       {"lang",
                        [<<"\"fra\"">>,<<"\"deu\"">>,<<"\"eng\"">>,<<"\"qad\"">>]}]}}
              ]},
             {<<"[*, {\"name\": \"France \\u00D4\", \"ip_multicast\":(?<ip>_)}, *]">>,
              [{MumuData,
                {true, [{"ip", [<<"\"239.100.10.4\"">>]}]}}
              ]}
            ],
    ejpet_test_helpers:generate_test_list(Tests).

-endif.
