-module(backup_watchdog).
-author("xvojtus").

-import(global, [whereis_name/1]).
-import(timer, [sleep/1]).

-record(state, {g_name, module, func, args, node}).
-export([start/5]).
-export([loop/1, init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start(Node, GlobalName, Module, Function, Args) ->
	State = #state{g_name=GlobalName, module=Module, func=Function, args=Args, node=Node},
  	init(State),
	spawn(Node, ?MODULE, loop, [State]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

init(State) ->
	apply(State#state.module, on_before_backup, [State#state.node]).

loop(State) ->
	error_logger:info_report("Watching on node ~p?~p process ~p.!!!~n", [node(), self(), State#state.g_name]),
	_Result = check_and_spawn(State),
	timer:sleep(5000),
	loop(State).

check_and_spawn(State) ->
	global:sync(),
	case global:whereis_name(State#state.g_name) of
		undefined ->
			error_logger:info_report("Process '~s' undefined. Trying to spawn one.!!!~n", [State#state.g_name]),
			spawn_it(State);
		_ ->
			continue
	end.

spawn_it(State) ->
	spawn(State#state.module, State#state.func, State#state.args), 
	global:sync(),
	case Pid = global:whereis_name(State#state.g_name) of 
		undefined ->
			couldnt_register;	
		_ ->
			error_logger:info_report("Registered ~p as '~p' with ~p!!!~n", [Pid, State#state.g_name, State]),
			spawned
	end.