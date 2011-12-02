-module(ucp_simulator_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 7777).
-define(TCP_OPTIONS, [binary,
                      {packet, 0},
                      {active, true},
                      {send_timeout, 5000},
                      {reuseaddr, true}]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Port = get_app_env(listen_port, ?DEFAULT_PORT),
    {ok, LSock} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    io:format("UCP server listening on port: ~p~n", [Port]),
    case ucp_simulator_sup:start_link(LSock) of
      {ok, Pid} ->
          ucp_simulator_sup:start_child(),
          {ok, Pid};
      Other ->
          {error, Other}
    end.

stop(_State) ->
    ok.

get_app_env(Opt, Default) ->
    case application:get_env(ucp_simulator, Opt) of
        {ok, Val} -> Val;
        _ -> Default
    end.
