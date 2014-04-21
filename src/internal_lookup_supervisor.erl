-module(internal_lookup_supervisor).
-include("internal_lookup.hrl").

-behaviour(supervisor).
-behaviour(backable).

-export([start/0]).

-export([start_up/0, on_before_backup/1, global_name/0]).

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
	 backable:backup(node(),  ?MODULE).

%%%===================================================================
%%% backable callbacks
%%%===================================================================

start_up() ->
	Ret = supervisor:start_link(?MODULE, []),
	case Ret of
		{ok, Pid} -> unlink(Pid)
	end,
	Ret.

on_before_backup(Node) ->
	io:format("initializing node '~p' ~n", [node()]),
	mnesia:delete_schema([node()]),
	case global:whereis_name(?SUPERVISOR_GLOBAL) of
		undefined ->
			on_master_start();
		Pid ->
			on_slave_start(node(Pid))
	end.

on_master_start() ->
	io:format("initializing as MASTER node ~p~n", [node()]),	
	mnesia:start(),
    mnesia:create_schema([node()]),
    mnesia:create_table(chrm, []).
	 

on_slave_start(MasterNode) ->
	io:format("initializing as SLAVE node ~p~n", [node()]),	
	mnesia:start(),
    mnesia:change_config(extra_db_nodes, [MasterNode]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    Tabs = mnesia:system_info(tables) -- [schema],
    [mnesia:add_table_copy(Tab,node(), disc_copies) || Tab <- Tabs].

global_name() ->
	?SUPERVISOR_GLOBAL.

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init(_Args) ->
    {ok, {{one_for_one, 1, 60},
          [{internal_lookup, {internal_lookup, start_link, []},
            permanent, brutal_kill, worker, [internal_lookup]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

