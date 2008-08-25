-module(yamd_cleaner).
-author('cooldaemon@gmail.com').

-include("yamd.hrl").

% External API
-export([start_link/0]). 

% Callbacks
-export([
  init/1,
  start_cleaning/0, cleaning/0, delete_loop/1
]).

% External API
start_link() -> proc_lib:start_link(?MODULE, init, [self()]).

% Callbacks
init(Parent) ->
  proc_lib:init_ack(Parent, {ok, self()}),
  start_cleaning(),
  cleaning().

start_cleaning() ->
  Result = yamd_store:set_cleaner(),
  if
    Result /= {atomic, ok} ->
      timer:sleep(?CLEANING_INTERVAL),
      start_cleaning();
    true -> ok
  end.

cleaning() ->
  case yamd_store:get_overdue_keys() of
    [] -> noexist;
    Keys -> delete_loop(Keys)
  end,
  timer:sleep(?CLEANING_INTERVAL),
  cleaning().

delete_loop([]) -> ok;
delete_loop([Key | Keys]) ->
  yamd_store:delete_overdue_record(Key),
  delete_loop(Keys).

