-module(yamd_tcp_client_sup).
-aauthor('cooldaemon@gmail.com').
-behaviour(supervisor).

-include("yamd.hrl").

% External API
-export([start_link/1, start_child/1]). 

% Callbacks
-export([init/1]). 

% External API
start_link(Module) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Module]).

start_child(Socket) ->
  supervisor:start_child(?MODULE, [Socket]).

% Callbacks
init([Module]) ->
  {ok, {{simple_one_for_one, ?MAX_RESTART, ?MAX_TIME}, [{
    undefined,
    {Module, start_link, []},
    temporary,
    ?SHUTDOWN_WAITING_TIME,
    worker,
    []
  }]}}.

