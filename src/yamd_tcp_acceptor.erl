-module(yamd_tcp_acceptor).
-author('cooldaemon@gmail.com').

-include("yamd.hrl").

% External API
-export([start_link/1]). 

% Callbacks
-export([init/2, accept/1]).

% External API
start_link(Port) ->
  proc_lib:start_link(?MODULE, init, [self(), Port]).

% Callbacks
init(Parent, Port) ->
  case gen_tcp:listen(
    Port, [{active, false}, binary, {packet, line}, {reuseaddr, true}]
  ) of
    {ok, ListenSocket} ->
      proc_lib:init_ack(Parent, {ok, self()}),
      accept(ListenSocket);
    {error, Reason} ->
      proc_lib:init_ack(Parent, {error, Reason}),
      ?LISTEN_ERR(Reason),
      error
    end.

accept(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  yamd_tcp_client_sup:start_child(Socket),
  accept(ListenSocket).

