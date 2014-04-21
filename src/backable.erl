-module(backable).
-author("xvojtus").


%% API
-export([backup/2,backup/3,backup/4]).

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


%%%===================================================================
%%% Internal functions
%%%===================================================================