-module(yamd_app).
-author('cooldaemon@gmail.com').
-behaviour(application).

-export([start/2, stop/1]). 

start(_Type, _Args) ->
  {ok, Port} = application:get_env(yamd, port),
  {ok, Type} = application:get_env(yamd, type),
  case application:get_env(yamd, mode) of
    {ok, start} ->
      yamd_sup:start_link([Port, Type]);
    {ok, copy} ->
      {ok, Node} = application:get_env(yamd, node),
      yamd_sup:start_link([Port, Type, Node])
  end.

stop(_State) -> ok.

