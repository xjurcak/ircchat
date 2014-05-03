-module(backable).
-author("xvojtus").


%% API
-export([backup/2,backup/3,backup/4,backup_slave/2,backup_slave/3,backup_slave/4]).

%% Behaviour
-export([behaviour_info/1]).
 
behaviour_info(callbacks) ->
    [{start_up, 0}, {on_before_backup, 1}, {global_name, 0}];
behaviour_info(_Other) ->
    undefined.
 
%%%===================================================================
%%% API
%%%===================================================================

backup(Node, Module) ->
	backup(Node, Module, start_up, []).

backup(Node, Module, Func) ->
	backup(Node, Module, Func, []).

backup(Node, Module, Func, Args) ->
	backup_watchdog:start(Node, Module:global_name(), Module, Func, Args). 
	
	
	
backup_slave(Node, Module) ->
	backup_slave(Node, Module, start_up).
backup_slave(Node, Module, _Func) ->
	backup_slave(Node, Module, start_up, []).
backup_slave(Node, Module, Func, Args) ->
	wait_for_global(Module),
	backup(Node, Module, Func, Args).


%%%===================================================================
%%% Internal functions
%%%===================================================================

wait_for_global(Module) ->
	wait_for_global(Module,10).
	
wait_for_global(_, 0) ->
	failed;
wait_for_global(Mod,N) ->
	case global:whereis_name(apply(Mod, global_name, [])) of
		undefined ->
			wait_for_global(Mod, N-1);
		_ ->
		ok
	end.