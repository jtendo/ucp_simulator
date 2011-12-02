-module(ucp_utils).
-author('rafal.galczynski@jtendo.com').
-author('andrzej.trawinski@jtendo.com').

-include("ucp_syntax.hrl").

-export([to_ira/1,
         from_ira/1,
         to_7bit/1,
         encode_sender/1,
         decode_sender/2,
         compose_message/2,
         get_next_trn/1,
         trn_to_str/1,
         decode_message/1,
         wrap/1,
         create_cmd_52/3]).

-define(STX, 16#02).
-define(ETX, 16#03).
-define(MIN_MESSAGE_TRN, 0).
-define(MAX_MESSAGE_TRN, 99).
-define(UCP_HEADER_LEN, 13).
-define(UCP_CHECKSUM_LEN, 2).
-define(UCP_SEPARATOR, $/).

%%--------------------------------------------------------------------
%% Convert string to string encoded into IRA,
%% after GSM 03.38 Version 5.3.0
%%--------------------------------------------------------------------
-spec to_ira(iolist()) -> iolist().
to_ira(Str) ->
    GsmMessage = lists:map(fun(X) -> ucp_ia5:ascii_to_gsm(X) end, Str),
    lists:flatten(GsmMessage).

from_ira(Str) ->
    ASCIMessage = lists:map(fun(X) -> ucp_ia5:gsm_to_ascii(X) end, Str),
    lists:flatten(ASCIMessage).

%%--------------------------------------------------------------------
%% Convert string to 7-bit encoding
%% according to: GSM 03.38 Version 5.3.0
%%--------------------------------------------------------------------
to_7bit(Str) -> binary:bin_to_list(ucp_7bit:to_7bit(Str)).

%%--------------------------------------------------------------------
%% Calculate UCP OAdC field
%%--------------------------------------------------------------------
encode_sender(Sender) ->
    % TODO: detect international numer and set OTOA: 1139
    case has_only_digits(Sender) of
        true ->
            {"", Sender};
        false ->
            {"5039", append_length(
                        hex:to_hexstr(
                           to_7bit(
                              to_ira(Sender))))}
    end.

decode_sender(OTOA, OAdC) ->
    case OTOA of
        "5039" ->
           % Cut off sender length
           from_ira(
              ucp_7bit:from_7bit(
                 hex:hexstr_to_list(
                    string:substr(OAdC, 3))));
        _Other ->
           OAdC
    end.

%%--------------------------------------------------------------------
%% Compose whole UCP message
%%--------------------------------------------------------------------
compose_message(Header, Body) ->
    HF = lists:nthtail(1, tuple_to_list(Header)),
    BF = lists:nthtail(1, tuple_to_list(Body)),
    Len = length(string:join(lists:concat([HF, BF]), "/")) + 3, % 3: for delimiter and CRC
    LenS = string:right(integer_to_list(Len, 10), 5, $0),
    % Update header with proper len value
    NewHeader = Header#ucp_header{len = LenS},
    NHF = lists:nthtail(1, tuple_to_list(NewHeader)),
    % Build message
    Message = string:concat(string:join(lists:concat([NHF, BF]), "/"), "/"),
    CRC = calculate_crc(Message),
    string:concat(Message, CRC).

%%--------------------------------------------------------------------
%% Append list length to begining of the list
%%--------------------------------------------------------------------
append_length(L) ->
    Fun = fun([H|_]) -> H == $0 end,
    {ElemsWithZero, ElemsWithOutZero} = lists:partition(Fun, L),
    Len = length(ElemsWithOutZero)*2
        + length(ElemsWithZero),
    HexStr = hex:to_hexstr(Len),
    lists:flatten([HexStr, L]).

%%--------------------------------------------------------------------
%% Get 8 last significant bits of number
%%--------------------------------------------------------------------
get_8lsb(Integer) ->
    Integer band 255.

%%--------------------------------------------------------------------
%% Calculate CRC checksum for UCP Message
%%--------------------------------------------------------------------
calculate_crc(Data) when is_list(Data) ->
    string:right(integer_to_list(get_8lsb(lists:sum(Data)), 16), 2, $0).

%%--------------------------------------------------------------------
%% Checks if char is digit
%%--------------------------------------------------------------------
is_digit(C) when C > 46, C < 58  -> true;
is_digit(_) -> false.

%%--------------------------------------------------------------------
%% Check if string contains only digits
%%--------------------------------------------------------------------
has_only_digits(Str) ->
    lists:all(fun(Elem) -> is_digit(Elem) end, Str).

%%--------------------------------------------------------------------
%% Increase and rotate TRN number
%%--------------------------------------------------------------------
get_next_trn(Val) when is_list(Val) ->
    get_next_trn(list_to_integer(Val));
get_next_trn(Val) when is_integer(Val) andalso Val >= ?MAX_MESSAGE_TRN ->
    ?MIN_MESSAGE_TRN;
get_next_trn(Val) when is_integer(Val) ->
    Val + 1.

%%--------------------------------------------------------------------
%% Right pad TRN number with zeros
%%--------------------------------------------------------------------
trn_to_str(Val) when is_integer(Val) ->
    string:right(integer_to_list(Val, 10), 2, $0).

%%--------------------------------------------------------------------
%% Decode binary into UCP message
%%--------------------------------------------------------------------
decode_message(Msg = <<?STX, BinHeader:?UCP_HEADER_LEN/binary, _/binary>>) ->
    Len = size(Msg) - 2,
    <<?STX, MsgS:Len/binary, ?ETX, Rest/binary>> = Msg,
    io:format("Received UCP message: ~s~n", [binary:bin_to_list(MsgS)]),
    % TODO: handle rest of the message
    case size(Rest) of
        0 ->
            HeaderList = binary:bin_to_list(BinHeader),
            case list_to_tuple(re:split(HeaderList, "/", [{return, list}])) of
                {TRN, LEN, OR, OT} ->
                    Header = #ucp_header{trn = TRN, len = LEN, o_r = OR, ot = OT},
                    BodyLen = erlang:list_to_integer(LEN) - ?UCP_HEADER_LEN - ?UCP_CHECKSUM_LEN - 2,
                    case Msg of
                        <<?STX, _Header:?UCP_HEADER_LEN/binary, ?UCP_SEPARATOR,
                        BinBody:BodyLen/binary, ?UCP_SEPARATOR,
                        _CheckSum:?UCP_CHECKSUM_LEN/binary, ?ETX>> ->
                            parse_body(Header, binary:bin_to_list(BinBody));
                        _ ->
                            {error, invalid_message}
                    end;
                _ ->
                    {error, invalid_header}
            end;
       _ -> {error, message_too_long}
    end;

decode_message(_) ->
    {error, invalid_message}.

%%--------------------------------------------------------------------
%% Parse UCP operations
%%--------------------------------------------------------------------
parse_body(Header = #ucp_header{ot = OT, o_r = "O"}, Data) ->
    case {OT, list_to_tuple(re:split(Data, "/", [{return, list}]))} of
        {"31", {ADC, PID}} ->
            Body = #ucp_cmd_31{adc = ADC, pid = PID},
            {ok, {Header, Body}};
        {"31", _} ->
            {error, invalid_command_syntax};
        {"60", {OADC, OTON, ONPI, STYP, PWD, NPWD, VERS, LADC, LTON, LNPI, OPID, RES1}} ->
            Body = #ucp_cmd_60{ oadc = OADC,
                           oton = OTON,
                           onpi = ONPI,
                           styp = STYP,
                           pwd = from_ira(PWD),
                           npwd = from_ira(NPWD),
                           vers = VERS,
                           ladc = LADC,
                           lton = LTON,
                           lnpi = LNPI,
                           opid = OPID,
                           res1 = RES1 },
            {ok, {Header, Body}};
        {"60", _} ->
            {error, invalid_command_syntax};
        {"51", {ADC, OADC, AC, NRQ, NADC, NT, NPID,
             LRQ, LRAD, LPID, DD, DDT, VP, RPID, SCTS, DST, RSN, DSCTS,
             MT, NB, MSG, MMS, PR, DCS, MCLS, RPI, CPG, RPLY, OTOA, HPLMN,
             XSER, RES4, RES5}} ->
             Body = #ucp_cmd_5x{adc=ADC, oadc=OADC, ac=AC, nrq=NRQ, nadc=NADC,
                          nt=NT, npid=NPID, lrq=LRQ, lrad=LRAD, lpid=LPID,
                          dd=DD, ddt=DDT, vp=VP, rpid=RPID, scts=SCTS,
                          dst=DST, rsn=RSN, dscts=DSCTS, mt=MT, nb=NB,
                          msg=MSG, mms=MMS, pr=PR, dcs=DCS, mcls=MCLS,
                          rpi=RPI, cpg=CPG, rply=RPLY, otoa=OTOA, hplmn=HPLMN,
                          xser=XSER, res4=RES4, res5=RES5},
            {ok, {Header, Body}};
        {"52", {ADC, OADC, AC, NRQ, NADC, NT, NPID,
             LRQ, LRAD, LPID, DD, DDT, VP, RPID, SCTS, DST, RSN, DSCTS,
             MT, NB, MSG, MMS, PR, DCS, MCLS, RPI, CPG, RPLY, OTOA, HPLMN,
             XSER, RES4, RES5}} ->
             Body = #ucp_cmd_5x{adc=ADC, oadc=OADC, ac=AC, nrq=NRQ, nadc=NADC,
                          nt=NT, npid=NPID, lrq=LRQ, lrad=LRAD, lpid=LPID,
                          dd=DD, ddt=DDT, vp=VP, rpid=RPID, scts=SCTS,
                          dst=DST, rsn=RSN, dscts=DSCTS, mt=MT, nb=NB,
                          msg=MSG, mms=MMS, pr=PR, dcs=DCS, mcls=MCLS,
                          rpi=RPI, cpg=CPG, rply=RPLY, otoa=OTOA, hplmn=HPLMN,
                          xser=XSER, res4=RES4, res5=RES5},
            {ok, {Header, Body}};
        {"53", {ADC, OADC, AC, NRQ, NADC, NT, NPID,
             LRQ, LRAD, LPID, DD, DDT, VP, RPID, SCTS, DST, RSN, DSCTS,
             MT, NB, MSG, MMS, PR, DCS, MCLS, RPI, CPG, RPLY, OTOA, HPLMN,
             XSER, RES4, RES5}} ->
             Body = #ucp_cmd_5x{adc=ADC, oadc=OADC, ac=AC, nrq=NRQ, nadc=NADC,
                          nt=NT, npid=NPID, lrq=LRQ, lrad=LRAD, lpid=LPID,
                          dd=DD, ddt=DDT, vp=VP, rpid=RPID, scts=SCTS,
                          dst=DST, rsn=RSN, dscts=DSCTS, mt=MT, nb=NB,
                          msg=MSG, mms=MMS, pr=PR, dcs=DCS, mcls=MCLS,
                          rpi=RPI, cpg=CPG, rply=RPLY, otoa=OTOA, hplmn=HPLMN,
                          xser=XSER, res4=RES4, res5=RES5},
            {ok, {Header, Body}};
        _ ->
            {error, unsupported_operation}
    end;


%%--------------------------------------------------------------------
%% Parse result messages
%%--------------------------------------------------------------------
parse_body(Header = #ucp_header{ot = OT, o_r = "R"}, Data) ->
    case {OT, list_to_tuple(re:split(Data, "/", [{return, list}]))} of
        {_OT, {"A", SM}} -> % OT: 31, 60
            Body = #ack{sm = SM},
            {ok, {Header, Body}};
        {_OT, {"A", MVP, SM}} -> % OT: 51
            Body = #ack{sm = SM, mvp = MVP},
            {ok, {Header, Body}};
        {_OT, {"N", EC, SM}} -> % OT: 31, 51, 60
            Body = #nack{ec = EC, sm = SM},
            {ok, {Header, Body}};
        _ ->
            {error, unsupported_operation}
    end;

parse_body(_Header, _Body) ->
    {error, unsupported_operation}.

%%--------------------------------------------------------------------
%% Wrap message into delimiters
%%--------------------------------------------------------------------
wrap(Message) ->
    binary:list_to_bin([?STX, Message, ?ETX]).

%%--------------------------------------------------------------------
%% Create delivery text short message notification
%%--------------------------------------------------------------------
create_cmd_52(Sender, Receiver, Message) when is_list(Message) ->
    PMsg = hex:to_hexstr(ucp_utils:to_ira(Message)),
    {otoa, OTOA, sender, UCPSender} = ucp_utils:calculate_sender(Sender),
    Body = #ucp_cmd_5x{
              oadc = UCPSender,
              adc = Receiver,
              otoa = OTOA,
              mt = "3",
              msg = PMsg},
    Header = #ucp_header{
              o_r = "O",
              ot = "52"},
    {ok, {Header, Body}}.

