-module(yamd_argument).
-author('cooldaemon@gmail.com').

-include("yamd.hrl").

-export([get/1, get/2]).

-export([
  validate_and_convert_name/1,
  validate_and_convert_port/1,
  validate_and_convert_type/1,
  validate_and_convert_node/1,
  validate_and_convert_message/1
]).

get(Mode) -> get(Mode, command_line).

get(Mode, RunType) ->
  Options = case Mode of
    start  -> get_argument(get_function([name, port, type]));
    stop   -> get_argument(get_function([node]));
    copy   -> get_argument(get_function([name, port, type, node]));
    chtype -> get_argument(get_function([type, node]));
    show   -> get_argument(get_function([message]))
  end,
  case proplists:get_value(error, Options) of
    undefined -> Options;
    _ ->
      say_error(RunType, Options),
      error
  end.

get_function(Names) when is_list(Names) ->
  [get_function(Name) || Name <- Names];
get_function(name) -> {name, fun yamd_argument:validate_and_convert_name/1};
get_function(port) -> {port, fun yamd_argument:validate_and_convert_port/1};
get_function(type) -> {type, fun yamd_argument:validate_and_convert_type/1};
get_function(node) -> {node, fun yamd_argument:validate_and_convert_node/1};
get_function(message) ->
  {message, fun yamd_argument:validate_and_convert_message/1}.

get_argument(Names) when is_list(Names) ->
  [get_argument(Name) || Name <- Names];

get_argument({Name, Func}) ->
  Option = case init:get_argument(Name) of
    {ok, [Values]} ->
      if
        length(Values) =:= 1 ->
          [Value] = Values,
          Func(Value);
        true -> Func(Values)
      end;
    error -> {badarg, "No input."}
  end,
  case Option of
    {badarg, Reason} -> {error, {Name, Reason}};
    _ -> {Name, Option}
  end.

validate_and_convert_name(Name) ->
  case node() of
    'nonode@nohost' -> {badarg, "No input"};
    _ -> Name
  end.

validate_and_convert_port(Port) ->
  case regexp:match(Port, "^[0-9]+$") of
    nomatch -> {badarg, "Not numeric."};
    _ ->
      Number = list_to_integer(Port),
      if
        Number < 1024 orelse 65534 < Number -> {badarg, "Out of range."};
        true -> Number
      end
  end.

validate_and_convert_type("ram")  -> ram_copies;
validate_and_convert_type("disc") -> disc_copies;
validate_and_convert_type(_)      -> {badarg, "Input ram or disc."}.

validate_and_convert_node(Node) ->
  Atom = list_to_atom(Node),
  case net_adm:ping(Atom) of
    pong -> Atom;
    _    -> {badarg, "Node not exists."}
  end.

validate_and_convert_message("info")    -> info_msg;
validate_and_convert_message("warning") -> warning_msg;
validate_and_convert_message("error")   -> error;
validate_and_convert_message(_) ->
  {badarg, "Input info, warning or error."}.

say_error(RunType, Options) ->
  lists:foreach(
    fun
      ({error, {Name, Reason}}) ->
        Message = io_lib:format(
          "error argument: ~p~nreason: ~s~n",
          [Name, Reason]
        ),
        case RunType of
          command_line -> io:fwrite(Message, []);
          _ -> ?ELOG(Message, [])
        end;
      (_) -> ok
    end,
    Options
  ).

