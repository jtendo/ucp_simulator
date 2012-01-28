-module(ucp_server).
-author('andrzej.trawinski@jtendo.com').

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% API
-export([start_link/1,
         stop/0,
         send_message/1]).

-include_lib("ucp_common/include/ucp_syntax.hrl").
-include("logger.hrl").

-define(SERVER, ?MODULE).

-record(state, {port,  % listening port
                lsock, % listening socket
                sock,  % socket
                trn,   % message number
                status}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

stop() ->
    gen_server:cast(?SERVER, stop).

send_message(Msg) ->
    gen_server:cast(?SERVER, {send_message, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([LSock]) ->
    ?SYS_INFO("Initializing UCP server~n", []),
    gen_server:cast(self(), accept),
    {ok, #state{lsock = LSock, trn = 0}}.

handle_call(Msg, From, State) ->
    ?SYS_WARN("Unknown call from (~p): ~p", [From, Msg]),
    {reply, {ok, Msg}, State}.

handle_cast(accept, State = #state{lsock = S}) ->
    {ok, Sock} = gen_tcp:accept(S),
    {noreply, State#state{sock = Sock}};

handle_cast({send_message, _Msg}, State) ->
    % TODO: implement this
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    handle_data(Socket, RawData),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    ?SYS_INFO("Connection closed by peer.", []),
    {stop, normal, State};
%handle_info(timeout, #state{lsock = LSock} = State) ->
    %{ok, Sock} = gen_tcp:accept(LSock),
    %ucp_simulator_sup:start_child(),
    %{noreply, State#state{sock = Sock}};
handle_info(Any, State) ->
    ?SYS_INFO("Unhandled message: ~p", [Any]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_data(Socket, RawData) ->
    try
        case handle_message(RawData) of
            {reply, {Header, Body}} ->
                Reply = ucp_utils:compose_message(Header, Body),
                ?SYS_INFO("Sending UCP reply: ~p", [Reply]),
                send(Socket, ucp_utils:wrap(Reply));
            noreply ->
                send(Socket, <<"Error">>)
        end
    catch
        _:_ ->
            ?SYS_ERROR("Error: ~p", [erlang:get_stacktrace()])
    end.

handle_message(RawData) ->
    Result = ucp_utils:decode_message(RawData),
    ?SYS_INFO("Parsed UCP message: ~p", [Result]),
    case Result of
        {ok, Message} ->
            process_message(Message);
        {error, {invalid_message_body, Header}} ->
            ?SYS_INFO("Error parsing message: invalid body", []),
            {reply, {Header#ucp_header{o_r = "R"}, #nack{ec = "02"}}};
        {error, {unsupported_operation, Header}} ->
            ?SYS_INFO("Error parsing message: unsupported operation", []),
            {reply, {Header#ucp_header{o_r = "R"}, #nack{ec = "04"}}};
        Error ->
            ?SYS_INFO("Error handling message: ~p", [Error]),
            noreply
    end.

process_message({Header = #ucp_header{ot = "31", o_r = "O"}, _Body}) ->
    {reply, {Header#ucp_header{o_r = "R"}, #short_ack{sm = "0000"}}};

process_message({Header = #ucp_header{ot = "51", o_r = "O"}, Body}) ->
    case Body#ucp_cmd_5x.mt of
        "3" ->
            % print out text message
            ?SYS_INFO("Text message: ~s", [ucp_utils:hexstr_to_list(Body#ucp_cmd_5x.msg)]);
        _ -> ignore
    end,
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_datetime(erlang:now()),
    SM = lists:flatten(io_lib:format("~s:~2B~2B~2B~2B~2B~2B", [Body#ucp_cmd_5x.adc, Day, Month, Year rem 100, Hour, Min, Sec])),
    {reply, {Header#ucp_header{o_r = "R"}, #ack{sm = SM}}};

process_message({Header = #ucp_header{ot = "60", o_r = "O"}, Body}) ->
    case Body#ucp_cmd_60.styp of
        "1" -> % Open session
            % Pass everyone
            % TODO: implement authorization
            {reply, {Header#ucp_header{o_r = "R"}, #short_ack{}}};
        Type -> {error, operation_subtype_not_supported, Type}
    end;

% Reply to other messages with ACK
process_message({Header = #ucp_header{o_r = "O"} , _Body}) ->
    % slow down reply
    timer:sleep(1000),
    {reply, {Header#ucp_header{o_r = "R"}, #short_ack{}}};

process_message({#ucp_header{o_r = "R"} , _Body}) ->
    noreply.

send(S, Msg) ->
    ok = gen_tcp:send(S, Msg),
    inet:setopts(S, [{active, once}]),
    ok.
