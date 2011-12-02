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

-include("ucp_syntax.hrl").

-ifdef(TEST).
-compile([export_all]).
-endif.

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
    {ok, #state{lsock = LSock, trn = 0}, 0}.

handle_call(Msg, _From, State) ->
    io:format("Unknown call: ~p~n", [Msg]),
    {reply, {ok, Msg}, State}.

handle_cast({send_message, Msg}, State) ->
    TRN = ucp_utils:get_next_trn(State),
    try
        {ok, {Header, Body}} = ucp_utils:create_cmd_52("123", "567", Msg),
        Reply = ucp_utils:compose_message(Header#ucp_header{trn = ucp_utils:trn_to_str(TRN)}, Body),
        io:format("Sending UCP message: ~p~n", [Reply]),
        gen_tcp:send(State#state.sock, ucp_utils:wrap(Reply))
    catch
        Class:Reason ->
            io:format("Error: ~p:~p~nStack: ~p~n", [Class, Reason, erlang:get_stacktrace()])
    end,
    {noreply, State#state{trn = TRN}};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    handle_data(Socket, RawData),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    io:format("Connection closed by peer.~n"),
    {stop, normal, State};
handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    ucp_simulator_sup:start_child(),
    {noreply, State#state{sock = Sock}};
handle_info(Any, State) ->
    io:format("Unhandled message: ~p~n", [Any]),
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
                io:format("Sending UCP reply: ~p~n", [Reply]),
                gen_tcp:send(Socket, ucp_utils:wrap(Reply));
            noreply ->
                ignore
        end
    catch
        _:_ ->
            io:format("Error: ~p~n", [erlang:get_stacktrace()])
    end.

handle_message(RawData) ->
    Result = ucp_utils:decode_message(RawData),
    io:format("Parsed UCP message: ~p~n", [Result]),
    case Result of
        {ok, Message} ->
            process_message(Message);
        _Error ->
            {reply, {#ucp_header{ot = "31", o_r = "R"}, #nack{ec = "02"}}}
    end.

process_message({Header = #ucp_header{ot = "31", o_r = "O"}, _Body}) ->
    {reply, {Header#ucp_header{o_r = "R"}, #short_ack{sm = "0000"}}};

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

