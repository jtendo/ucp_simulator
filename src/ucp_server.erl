-module(ucp_server).

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

%%--------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link(Port::integer()) -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%--------------------------------------------------------------------
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

send_message(Msg) ->
    gen_server:cast(?SERVER, {send_message, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([LSock]) ->
    {ok, #state{lsock = LSock, trn = 0}, 0}.

handle_call(Msg, _From, State) ->
    io:format("Unknown call: ~p~n", [Msg]),
    {reply, {ok, Msg}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({send_message, Msg}, State) ->
    TRN = ucp_utils:get_next_trn(State),
    try
        {ok, {Header, Body}} = ucp_utils:create_cmd_52("123", "567", Msg),
        Reply = ucp_utils:compose_message(Header#ucp_header{trn = ucp_utils:trn_to_str(TRN)}, Body),
        io:format("Sending UCP message: ~p~n", [Reply]),
        gen_tcp:send(State#state.sock, binary:list_to_bin([?STX, Reply, ?ETX]))
    catch
        Class:Reason ->
            io:format("Error: ~p:~p~nStack: ~p~n", [Class, Reason, erlang:get_stacktrace()])
    end,
    {noreply, State#state{trn = TRN}};

handle_cast(stop, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
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
                gen_tcp:send(Socket, binary:list_to_bin([?STX, Reply, ?ETX]));
            noreply ->
                ignore
        end
    catch
        _:_ ->
            io:format("Error: ~p~n", [erlang:get_stacktrace()])
    end.

handle_message(RawData) ->
    Result = ucp_utils:decode_message(RawData),
    io:format("Parsing result: ~p~n", [Result]),
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
            {reply, {Header#ucp_header{o_r = "R"}, #short_ack{}}};
        Type -> {error, operation_subtype_not_supported, Type}
    end;

% Reply to other messages with ACK
process_message({Header = #ucp_header{o_r = "O"} , _Body}) ->
    timer:sleep(2000),
    {reply, {Header#ucp_header{o_r = "R"}, #short_ack{}}};

process_message({#ucp_header{o_r = "R"} , _Body}) ->
    noreply.

