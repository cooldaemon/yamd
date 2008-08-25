-module(yamd_test).
-author('cooldaemon@gmail.com').

-include("eunit.hrl").

-export([store_test_expire/0]).

store_test_() ->
  yamd_store_manager:start_link([ram_copies]),
  [
    ?_assert(yamd_store:get("key0") == {atomic, noexists}),

    ?_assert(yamd_store:set(normal, "key0", "value0", 0) == {atomic, ok}),
    ?_assert(yamd_store:get("key0") == {atomic, "value0"}),

    ?_assert(yamd_store:set(add, "key1", "value1", 0) == {atomic, ok}),
    ?_assert(yamd_store:set(add, "key1", "value1", 0) == {aborted, exists}),
    ?_assert(yamd_store:get("key1") == {atomic, "value1"}),
    
    ?_assert(yamd_store:set(replace, "key0", "value2", 0) == {atomic, ok}),
    ?_assert(
      yamd_store:set(replace, "key2", "value", 0) == {aborted, noexists}
    ),
    ?_assert(yamd_store:get("key0") == {atomic, "value2"}),

    ?_assert(
      yamd_store:set(normal, {"name_space", "key"}, {0, "value"}, 0)
        == {atomic, ok}
    ),
    ?_assert(
      yamd_store:get({"name_space", "key"})
        == {atomic, {0, "value"}}
    ),

    ?_assert(store_test_expire() == {atomic, noexists})
  ].

store_test_expire() ->
  yamd_store:set(normal, "key3", "value", 1),
  timer:sleep(1001),
  yamd_store:get("key3").

