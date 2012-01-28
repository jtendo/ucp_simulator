-module(ucp_simulator_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

-include("logger.hrl").

start() ->
    application:start(ucp_simulator).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case ucp_simulator_sup:start_link() of
      {ok, _} ->
          ucp_simulator_sup:start_child();
      Other ->
          {error, Other}
    end.

stop(_State) ->
    ok.

