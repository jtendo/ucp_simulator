-module(ucp_simulator).
-license("New BSD License, see LICENSE for details").

-export([start/0]).

%% @doc Start the application. Mainly useful for using `-s ucp_simulator' as a command
%% line switch to the VM to make ucp_simulator start on boot.
start() ->
    application:start(ucp_simulator).

