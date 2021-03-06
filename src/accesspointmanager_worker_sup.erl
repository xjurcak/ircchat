-module(accesspointmanager_worker_sup).
-export([start_link/0, init/1]).
-behaviour(supervisor).

start_link() ->
  supervisor:start_link(?MODULE, []).

init(_Args) ->
  {ok, {{simple_one_for_one, 5, 3600},
    [{ppool_worker,
      {accesspoint, start_link , []},
      temporary, 5000, worker, [accesspoint]}]}}.
    