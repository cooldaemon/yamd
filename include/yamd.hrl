-author('cooldaemon@gmail.com').

-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).
-define(SHUTDOWN_WAITING_TIME, 2000).

-define(CLEANING_INTERVAL, 600000).
-define(UNLIMITED, 0).

-define(FAIL(Message), io_lib:format("FAILURE\r\n~s\r\n", [Message])).

-define(ELOG(Message, Args), error_logger:error_msg(
  "node: ~p~nmodule: ~p~nline: ~p~n" ++ Message,
  [node(), ?MODULE, ?LINE | Args]
)).

-define(LISTEN_ERR(Reason), ?ELOG(
  "gen_tcp:listen/2 returned error\n~p\n", [Reason]
)).

-define(RECV_ERR(Socket, Error), ?ELOG(
  "gen_tcp:recv/2 returned error.\nSocket:~p\nError:~p\n",
  [Socket, Error]
)).

-define(STORE_ERR(Method, Error), ?ELOG(
  "yamd_store:~s returned error.\nError:~p\n",
  [Method, Error]
)).

-record(store, {key, value, expire=?UNLIMITED}).
-record(information, {key, value}).

