-module(ucp_simulator_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/0]).
-export([init/1]).

-define(DEFAULT_PORT, 7777).
-define(TCP_OPTIONS, [binary,
                      {packet, 0},
                      {active, once},
                      {reuseaddr, true}]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Port = get_app_env(listen_port, ?DEFAULT_PORT),
    {ok, ListenSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    {ok, {{simple_one_for_one, 10, 60},
         [{ucp_server,
          {ucp_server, start_link, [ListenSocket]},
          temporary, 1000, worker, [ucp_server]}
         ]}}.

start_child() ->
    supervisor:start_child(?MODULE, []).

get_app_env(Opt, Default) ->
    case application:get_env(ucp_simulator, Opt) of
        {ok, Val} -> Val;
        _ -> Default
    end.
