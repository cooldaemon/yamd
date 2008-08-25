-module(yamd_tcp_interface).
-author('cooldaemon@gmail.com').

-include("yamd.hrl").

% External API
-export([start_link/1]). 

% Callbacks
-export([init/2, handle_connection/1, process_command/2]).

% External API
start_link(Socket) ->
  proc_lib:start_link(?MODULE, init, [self(), Socket]).

% Callbacks
init(Parent, Socket) ->
  proc_lib:init_ack(Parent, {ok, self()}),
  handle_connection(Socket).

handle_connection(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, B} ->
      Token = string:tokens(binary_to_list(B), " \r\n"),
      process_command(Socket, Token),
      handle_connection(Socket);
    {error, closed} ->
      ok;
    RecvError ->
      ?RECV_ERR(Socket, RecvError)
  end.

process_command(Socket, ["get", Key]) ->
  Message = case yamd_store:get(Key) of
   {atomic, noexists} ->
     ?FAIL("no exists");
   {atomic, Value} ->
     io_lib:format(
       "SUCCESS ~s ~w\r\n~s\r\n",
       [Key, string:len(Value), Value]
     );
   GetError ->
     ?STORE_ERR("get/1", GetError),
     ?FAIL("get error")
  end,
  gen_tcp:send(Socket, Message);

process_command(Socket, [Command, Key, Expire, Bytes]) ->
  inet:setopts(Socket, [{packet, raw}]),
  Result = case gen_tcp:recv(Socket, list_to_integer(Bytes)) of
    {ok, Value} ->
      case yamd_store:set(
        list_to_atom(Command),
        Key,
        binary_to_list(Value),
        list_to_integer(Expire)
      ) of
        {atomic, ok} ->
          "SUCCESS\r\n";
        {aborted, Message} ->
          ?FAIL(atom_to_list(Message));
        SetError ->
          ?STORE_ERR("set/4", SetError),
          ?FAIL("set error")
      end;
    {error, closed} ->
      closed;
    RecvError ->
      ?RECV_ERR(Socket, RecvError),
      ?FAIL("recv error")
  end,

  if Result /= closed ->
    gen_tcp:recv(Socket, 2),
    inet:setopts(Socket, [{packet, line}]),
    gen_tcp:send(Socket, Result)
  end;

process_command(Socket, ["delete", Key]) ->
  Result = case yamd_store:delete(Key) of
    {atomic, ok} ->
      "SUCCESS\r\n";
    {aborted, Message} ->
      ?FAIL(atom_to_list(Message));
    DeleteError ->
      ?STORE_ERR("delete/1", DeleteError),
      ?FAIL("delete error")
  end,
  gen_tcp:send(Socket, Result);

process_command(Socket, [Command]) when Command =:= "quit" orelse Command =:= "exit" ->
  gen_tcp:close(Socket);

process_command(Socket, _) ->
  gen_tcp:send(Socket, ?FAIL("format error")).

