-module(starter).

%% API
-export([start/2,start/3]).

%% Behaviour
-export([behaviour_info/1]).
 
behaviour_info(callbacks) ->
    [{start, 0}];
behaviour_info(_Other) ->
    undefined.

start(Module, Node) ->
	start(Module, Node, 10).
start(_Module, _Node, 0) ->
	{error, couldnt_connect_to_node};
start(Module, Node, Count) ->
	case net_adm:ping(Node) of
		pong ->
			global:sync(), %% wait till synced data
			apply(Module, start, []);
		pang ->
			timer:sleep(5000),
			start(Node, Count-1)
	end.