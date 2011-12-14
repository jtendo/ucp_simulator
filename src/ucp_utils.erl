-module(ucp_utils).
-author('rafal.galczynski@jtendo.com').
-author('andrzej.trawinski@jtendo.com').
-author('adam.rutkowski@jtendo.com').

-include("ucp_syntax.hrl").

-export([decode_sender/2,
         compose_message/2,
         decode_message/1,
         wrap/1]).

-export([to_hexstr/1,
         hexstr_to_bin/1,
         hexstr_to_list/1]).

-define(STX, 16#02).
-define(ETX, 16#03).
-define(MIN_MESSAGE_TRN, 0).
-define(MAX_MESSAGE_TRN, 99).
-define(UCP_HEADER_LEN, 13).
-define(UCP_CHECKSUM_LEN, 2).
-define(UCP_SEPARATOR, $/).

%%--------------------------------------------------------------------
%% Convert string encoded in IRA into ASCII,
%%--------------------------------------------------------------------
from_ira(Str) ->
    ASCIMessage = lists:map(fun(X) -> ucp_ia5:gsm_to_ascii(X) end, Str),
    lists:flatten(ASCIMessage).

%%--------------------------------------------------------------------
%% Decode UCP OAdC field
%%--------------------------------------------------------------------
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
    case {OT, field_split(Data)} of
        {"31", [ADC, PID]} ->
            Body = #ucp_cmd_31{adc = ADC, pid = PID},
            {ok, {Header, Body}};
        {"31", _} ->
            {error, {syntax_error, Header}};
        {"60", [OADC, OTON, ONPI, STYP, PWD, NPWD, VERS, LADC, LTON, LNPI, OPID, RES1]} ->
            Body = #ucp_cmd_60{ oadc = OADC,
                           oton = OTON,
                           onpi = ONPI,
                           styp = STYP,
                           pwd = ucp_ira:to(ascii, PWD),
                           npwd = ucp_ira:to(ascii, NPWD),
                           vers = VERS,
                           ladc = LADC,
                           lton = LTON,
                           lnpi = LNPI,
                           opid = OPID,
                           res1 = RES1 },
            {ok, {Header, Body}};
        {"60", _} ->
            {error, {syntax_error, Header}};
        {"51", [ADC, OADC, AC, NRQ, NADC, NT, NPID,
             LRQ, LRAD, LPID, DD, DDT, VP, RPID, SCTS, DST, RSN, DSCTS,
             MT, NB, MSG, MMS, PR, DCS, MCLS, RPI, CPG, RPLY, OTOA, HPLMN,
             XSER, RES4, RES5]} ->
             Body = #ucp_cmd_5x{adc=ADC, oadc=OADC, ac=AC, nrq=NRQ, nadc=NADC,
                          nt=NT, npid=NPID, lrq=LRQ, lrad=LRAD, lpid=LPID,
                          dd=DD, ddt=DDT, vp=VP, rpid=RPID, scts=SCTS,
                          dst=DST, rsn=RSN, dscts=DSCTS, mt=MT, nb=NB,
                          msg=MSG, mms=MMS, pr=PR, dcs=DCS, mcls=MCLS,
                          rpi=RPI, cpg=CPG, rply=RPLY, otoa=OTOA, hplmn=HPLMN,
                          xser=XSER, res4=RES4, res5=RES5},
            {ok, {Header, Body}};
        {"51", _} ->
            {error, {syntax_error, Header}};
        {"52", [ADC, OADC, AC, NRQ, NADC, NT, NPID,
             LRQ, LRAD, LPID, DD, DDT, VP, RPID, SCTS, DST, RSN, DSCTS,
             MT, NB, MSG, MMS, PR, DCS, MCLS, RPI, CPG, RPLY, OTOA, HPLMN,
             XSER, RES4, RES5]} ->
             Body = #ucp_cmd_5x{adc=ADC, oadc=OADC, ac=AC, nrq=NRQ, nadc=NADC,
                          nt=NT, npid=NPID, lrq=LRQ, lrad=LRAD, lpid=LPID,
                          dd=DD, ddt=DDT, vp=VP, rpid=RPID, scts=SCTS,
                          dst=DST, rsn=RSN, dscts=DSCTS, mt=MT, nb=NB,
                          msg=MSG, mms=MMS, pr=PR, dcs=DCS, mcls=MCLS,
                          rpi=RPI, cpg=CPG, rply=RPLY, otoa=OTOA, hplmn=HPLMN,
                          xser=XSER, res4=RES4, res5=RES5},
            {ok, {Header, Body}};
        {"52", _} ->
            {error, {syntax_error, Header}};
        {"53", [ADC, OADC, AC, NRQ, NADC, NT, NPID,
             LRQ, LRAD, LPID, DD, DDT, VP, RPID, SCTS, DST, RSN, DSCTS,
             MT, NB, MSG, MMS, PR, DCS, MCLS, RPI, CPG, RPLY, OTOA, HPLMN,
             XSER, RES4, RES5]} ->
             Body = #ucp_cmd_5x{adc=ADC, oadc=OADC, ac=AC, nrq=NRQ, nadc=NADC,
                          nt=NT, npid=NPID, lrq=LRQ, lrad=LRAD, lpid=LPID,
                          dd=DD, ddt=DDT, vp=VP, rpid=RPID, scts=SCTS,
                          dst=DST, rsn=RSN, dscts=DSCTS, mt=MT, nb=NB,
                          msg=MSG, mms=MMS, pr=PR, dcs=DCS, mcls=MCLS,
                          rpi=RPI, cpg=CPG, rply=RPLY, otoa=OTOA, hplmn=HPLMN,
                          xser=XSER, res4=RES4, res5=RES5},
            {ok, {Header, Body}};
        {"53", _} ->
            {error, {syntax_error, Header}};
        _ ->
            {error, {unsupported_operation, Header}}
    end;


%%--------------------------------------------------------------------
%% Parse result messages
%%--------------------------------------------------------------------
parse_body(Header = #ucp_header{ot = OT, o_r = "R"}, Data) ->
    case {OT, field_split(Data)} of
        {_OT, ["A", SM]} -> % OT: 31, 60
            Body = #ack{sm = SM},
            {ok, {Header, Body}};
        {_OT, ["A", MVP, SM]} -> % OT: 51
            Body = #ack{sm = SM, mvp = MVP},
            {ok, {Header, Body}};
        {_OT, ["N", EC, SM]} -> % OT: 31, 51, 60
            Body = #nack{ec = EC, sm = SM},
            {ok, {Header, Body}};
        _ ->
            {error, unsupported_operation}
    end;

parse_body(_Header, _Body) ->
    {error, unsupported_operation}.

%%--------------------------------------------------------------------
%% Utility functions
%%--------------------------------------------------------------------

wrap(Message) ->
    binary:list_to_bin([?STX, Message, ?ETX]).

field_split(L) ->
    re:split(L, [?UCP_SEPARATOR], [{return,list}]).

%%--------------------------------------------------------------------
%% Hex mangling utils
%%--------------------------------------------------------------------

to_hexstr(Bin) when is_binary(Bin) ->
    to_hexstr(binary_to_list(Bin));

to_hexstr(Int) when is_integer(Int) andalso Int > 255 ->
    to_hexstr(unicode, Int);

to_hexstr(Int) when is_integer(Int) ->
    to_hexstr(ascii, Int);

to_hexstr(L) when is_list(L) ->
    Type = case lists:any(fun(X) when X > 255 ->
                    true;
                   (_) ->
                    false
                   end, L) of
              true -> unicode;
              false -> ascii
          end,
    lists:flatten([to_hexstr(Type, X) || X <- L]).

hexstr_to_bin(H) ->
    <<<<(erlang:list_to_integer([X], 16)):4>> || X <- H>>.

hexstr_to_list(H) ->
    binary_to_list(hexstr_to_bin(H)).

to_hexstr(ascii, Int) when is_integer(Int) ->
    string:right(integer_to_list(Int, 16), 2, $0);

to_hexstr(unicode, Int) when is_integer(Int) ->
    string:right(integer_to_list(Int, 16), 4, $0).
