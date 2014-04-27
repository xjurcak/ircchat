-module(chatroommanager_worker_sup).
-export([start_link/0, init/1]).
-behaviour(supervisor).

start_link() ->
  supervisor:start_link(?MODULE, []).

init(_Arg) ->
  {ok, {{simple_one_for_one, 5, 3600},
    [{ppool_worker,
      {chatroom, start_link , []},
      temporary, 5000, worker, [chatroom]}]}}.
    