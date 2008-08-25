-module(yamd_ctl).
-author('cooldaemon@gmail.com').

-export([start/0, copy/0, stop/0, change_copy_type/0, show/0]).

start() -> app_start(start).
copy()  -> app_start(copy).

app_start(Mode) ->
  case yamd_argument:get(Mode, daemon) of
    error ->
      init:stop(),
      error;
    Options ->
      Names = case Mode of
        start -> [port, type];
        _     -> [port, type, node]
      end,
      lists:foreach(get_setenv_fun(Options), Names),
      application:set_env(yamd, mode, Mode),
      application:start(yamd, permanent)
  end.

get_setenv_fun(Options) ->
  fun (Name) ->
    application:set_env(
      yamd,
      Name,
      proplists:get_value(Name, Options)
    )
  end.

stop() ->
  case yamd_argument:get(stop) of
    error -> error;
    Options ->
      Node = proplists:get_value(node, Options),
      put_message(rpc:call(Node, init, stop, []), "Stopping.")
  end.

change_copy_type() ->
  case yamd_argument:get(chtype) of
    error -> error;
    Options ->
      Node = proplists:get_value(node, Options),
      Type = proplists:get_value(type, Options),
      put_message(
        rpc:call(Node, yamd_store, change_copy_type, [Type]),
        "Changing storage type."
      )
  end.

show() ->
  case yamd_argument:get(show) of
    error -> error;
    Options ->
      Message = proplists:get_value(message, Options),
      rb:start(),
      rb:show(Message)
  end.

put_message(Result, Message) ->
  case Result of
    ok -> io:fwrite("~s~n", [Message]), ok;
    _  -> error
  end.

