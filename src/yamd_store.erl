-module(yamd_store).
-author('cooldaemon@gmail.com').

-include("yamd.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([
  start/1, start/2, stop/0, change_copy_type/1,
  get/1, set/4, async_set/4, delete/1, async_delete/1,
  get_overdue_keys/0, delete_overdue_record/1,
  set_cleaner/0, write_cleaner/1
]).

start(ram_copies) ->
  case mnesia:system_info(use_dir) of
    true -> schema_exists_on_disc;
    _    -> start_and_create_tables(ram_copies)
  end;

start(disc_copies) ->
  case mnesia:system_info(use_dir) of
    true ->
      mnesia:start(),
      {atomic, ok};
    _ ->
      mnesia:create_schema([node()]),
      start_and_create_tables(disc_copies)
  end.

start_and_create_tables(CopyType) ->
  mnesia:start(),
  create_table(store, record_info(fields, store), CopyType),
  create_table(information, record_info(fields, information), CopyType).

create_table(Name, RecordInfo, CopyType) ->
  DiscCopies = case CopyType of
    disc_copies -> [{disc_copies, [node()]}];
    _           -> []
  end,
  mnesia:create_table(
    Name,
    lists:append([{attributes, RecordInfo}], DiscCopies)
  ).

start(CopyType, Node) ->
  case net_adm:ping(Node) of
    pong -> case mnesia:system_info(use_dir) of
              true -> schema_exists_on_disc;
              _    -> start_and_copy_tables(CopyType, Node)
            end;
    _    -> node_noexists
  end.
  
start_and_copy_tables(CopyType, Node) ->
  mnesia:start(),
  mnesia:change_config(extra_db_nodes, [Node]),
  case CopyType of
    disc_copies -> mnesia:change_table_copy_type(schema, node(), CopyType);
    _           -> ok
  end,
  case lists:any(
    fun (store) -> true; (_) -> false end,
    mnesia:system_info(local_tables)
  ) of
    false ->
      lists:foreach(fun (Name) ->
        mnesia:add_table_copy(Name, node(), CopyType)
      end, [store, information]),
      {atomic, ok};
    _ ->
      change_copy_type(CopyType)
  end.

stop() ->
  mnesia:stop().

change_copy_type(CopyType) ->
  Names = [schema, store, information],
  SortNames = case CopyType of
    ram_copies -> lists:reverse(Names);
    _          -> Names
  end,
  Node = node(),
  lists:foreach(fun (Name) ->
    case
      lists:any(
        fun (X) -> case Node of X -> true; _ -> false end end,
          mnesia:table_info(Name, CopyType)
      )
    of
      true -> ok;
      _    -> mnesia:change_table_copy_type(Name, node(), CopyType)
    end
  end, SortNames),
  {atomic, ok}.

microsecs() ->
  {MegaSecs, Secs, MicroSecs} = now(),
  MegaSecs * 1000000000000 + Secs * 1000000 + MicroSecs.

retrieval(Key, LockKind) ->
  case mnesia:read(store, Key, LockKind) of
    [] -> noexists;
    [{store, _, Value, ?UNLIMITED}] -> Value;
    [{store, _, Value, Expire}] ->
      Now = microsecs(),
      if
        Expire > Now -> Value;
        true -> noexists
      end;
    Error ->
      ?STORE_ERR("mnesia:read/3", Error),
      error
  end.

get(Key) ->
  F = fun() -> retrieval(Key, read) end,
  mnesia:transaction(F).

set(Mode, Key, Value, Expire) ->
  SetExpire = case Expire of
    ?UNLIMITED -> ?UNLIMITED;
    _ -> microsecs() + Expire * 1000000
  end,
  Row = #store{key=Key, value=Value, expire=SetExpire},
  F = case Mode of
    add ->
      fun() ->
        case retrieval(Key, write) of
          noexists -> mnesia:write(Row);
          error -> mnesia:abort(error);
          _ -> mnesia:abort(exists)
        end
      end;
    replace ->
      fun() ->
        case retrieval(Key, write) of
          noexists -> mnesia:abort(noexists);
          error -> mnesia:abort(error);
          _ -> mnesia:write(Row)
        end
      end;
    _ -> fun() -> mnesia:write(Row) end
  end,
  mnesia:transaction(F).

async_set(Mode, Key, Value, Expire) ->
  spawn(?MODULE, set, [Mode, Key, Value, Expire]).

delete(Key) ->
  Oid = {store, Key},
  F = fun() -> mnesia:delete(Oid) end,
  mnesia:transaction(F).

async_delete(Key) ->
  spawn(?MODULE, delete, [Key]).

get_overdue_keys() ->
  Now = microsecs(),
  mnesia:dirty_select(store ,[{
    {store, '$1', '_', '$2'},
    [{'/=', '$2', ?UNLIMITED}, {'<', '$2', Now}],
    ['$1']
  }]).

delete_overdue_record(Key) ->
  F = fun () ->
    case mnesia:read(store, Key, write) of
      [] -> mnesia:abort(noexists);
      [{store, _Key, _Value, ?UNLIMITED}] -> mnesia:abort(unlimited);
      [{store, _Key, _Value, Expire}] ->
        Now = microsecs(),
        if
          Expire > Now -> mnesia:abort(doesnot_expire);
          true -> mnesia:delete({store, Key})
        end;
      Error ->
        ?STORE_ERR("mnesia:read/3", Error),
        mnesia:abort(error)
    end
  end,
  mnesia:transaction(F).

set_cleaner() ->
  Node = node(),
  F = fun () ->
    case mnesia:read(information, cleaner, write) of
      [] -> write_cleaner(Node);
      [{information, _, CleanerNode}] ->
        case CleanerNode of
          Node -> ok;
          _ ->
            case net_adm:ping(CleanerNode) of
              pang -> write_cleaner(Node);
              _ -> node_exists
            end
        end;
      Error ->
        ?STORE_ERR("mnesia:read/3", Error),
        error
    end
  end,
  mnesia:transaction(F).

write_cleaner(Node) ->
  Row = #information{key=cleaner, value=Node},
  case mnesia:write(Row) of
    ok -> ok;
    Error ->
      ?STORE_ERR("mnesia:write/1", Error),
      error
  end.
 
