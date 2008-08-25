-module(yamd_store_manager).
-author('cooldaemon@gmail.com').

-behaviour(gen_server).

%% External API
-export([start_link/1]).

%% Callbacks
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

%% External API
start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% Callbacks
init(Args) ->
  process_flag(trap_exit, true),
  case apply(yamd_store, start, Args) of
    {atomic, ok} -> {ok, started};
    Error        -> {stop, Error}
  end. 

handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  yamd_store:stop(),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

