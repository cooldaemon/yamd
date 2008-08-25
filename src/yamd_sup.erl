-module(yamd_sup).
-author('cooldaemon@gmail.com').
-behaviour(supervisor).

-include("yamd.hrl").

% External API
-export([start_link/1, stop/0]). 

% Callbacks
-export([init/1]). 

% External API
start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

stop() ->
  case whereis(?MODULE) of
    Pid when pid(Pid) ->
      exit(Pid, shutdown),
      yamd_store:stop(),
      ok;
    _ -> not_started
  end.

% Callbacks
init([Port, Type]) -> get_init_result(Port, [Type]);
init([Port, Type, Node]) -> get_init_result(Port, [Type, Node]).

% Internal functions
get_init_result(Port, StoreArgs) ->
  Flags = {one_for_one, 0, 1},
  Children = [
    worker_spec(yamd_store_manager, [StoreArgs]),
    worker_spec(yamd_cleaner, []),
    worker_spec(yamd_tcp_acceptor, [Port]),
    supervisor_spec(yamd_tcp_client_sup, [yamd_tcp_interface])
  ],
  {ok, {Flags, Children}}.

worker_spec(Module, Args) ->
  StartFunc = {Module, start_link, Args},
  {Module, StartFunc, permanent, ?SHUTDOWN_WAITING_TIME, worker, [Module]}.

supervisor_spec(Module, Args) ->
  StartFunc = {Module, start_link, Args},
  {Module, StartFunc, permanent, infinity, supervisor, []}.

