-module(lookup_sup).
-include("lookup.hrl").

-behaviour(supervisor).
-behaviour(backable).

-export([start_slave/0, start/0]).

-export([start_up/0, on_before_backup/1, global_name/0]).

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
	 backable:backup(node(),  ?MODULE).

start_slave() ->
	 backable:backup_slave(node(),  ?MODULE).	

%%%===================================================================
%%% backable callbacks
%%%===================================================================

start_up() ->
	Ret = {ok, Pid} = supervisor:start_link({global, global_name()}, ?MODULE, []),
	unlink(Pid),
	Ret.

on_before_backup(_Node) ->
	error_logger:info_report("initializing node '~p' ~n", [node()]),
	mnesia:delete_schema([node()]),
	case global:whereis_name(global_name()) of
		undefined ->
			on_master_start();
		Pid ->
			on_slave_start(node(Pid))
	end.

on_master_start() ->
	error_logger:info_report("initializing as MASTER node ~p~n", [node()]),	
	mnesia:start(),
    mnesia:create_schema([node()]),
    mnesia:create_table(?MANAGERS_TABLE, []).
	 

on_slave_start(MasterNode) ->
	error_logger:info_report("initializing as SLAVE node ~p~n", [node()]),	
	mnesia:start(),
    mnesia:change_config(extra_db_nodes, [MasterNode]),
    Tabs = mnesia:system_info(tables),
    [mnesia:add_table_copy(Tab,node(), disc_copies) || Tab <- Tabs].

global_name() ->
	?LOOKUP_SERVER_SUP_GLOBAL.

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init(_Args) ->
    {ok, {{one_for_one, 1, 60},
          [{lookup, {lookup, start_link, []},
            permanent, brutal_kill, worker, [lookup]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
