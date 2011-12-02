-module(ucp_conn_tests).
-include_lib("eunit/include/eunit.hrl").
-include("ucp_syntax.hrl").
-compile([export_all]).


decode_ucp_cmd_60_test() ->
    %% "01/00059/O/60/2147/6/5/1/4E5053323134374E5053//0100//////55"
    Msg = <<2,48,49,47,48,48,48,53,57,47,79,47,54,48,47,50,49,52,55,47,54,47,53,47,49,47,52,69,53,48,53,51,51,50,51,49,51,52,51,55,52,69,53,48,53,51,47,47,48,49,48,48,47,47,47,47,47,47,53,53,3>>,
    ?assertEqual(#ucp_header{}, smsc_server:parse_message(Msg)).


