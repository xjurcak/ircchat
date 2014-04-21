-module(internal_lookup).
-behaviour(gen_server).


%% API
-export([start/0, start/1, start/2, start_link/0]).
-export([register_chrm/1, unregister_chrm/1, get_chrm/0, exists/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("internal_lookup.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
	
exists() ->
	case global:whereis_name(?SERVER_GLOBAL) of
		undefined ->
			false;
		_ ->
			true
	end.

register_chrm(Pid) -> 
	gen_server:call({global, ?SERVER_GLOBAL}, {register_chrm, Pid}).

unregister_chrm(Pid) -> 
	gen_server:call({global, ?SERVER_GLOBAL}, {unregister_chrm, Pid}).

get_chrm() -> 
	gen_server:call({global, ?SERVER_GLOBAL}, {get_chrm}).
	

start(Node) ->
	start(Node, 10).

start(_Node, 0) ->
	{error, couldnt_connect_to_node};

start(Node, Count) ->
	case net_adm:ping(Node) of
		pong ->
			timer:sleep(2000), %% wait till synced data
			internal_lookup_supervisor:start();
		pang ->
			timer:sleep(5000),
			start(Node, Count-1)
	end.

start() ->
	internal_lookup_supervisor:start().

% should not be used for server start!!!
start_link() ->
  gen_server:start_link({global, ?SERVER_GLOBAL}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  init_i(),
  {ok, #state{}}.

handle_call({hello}, _From, State) ->
  {reply, {ok, self()}, State};

handle_call({register_chrm, Pid}, _From, State) ->
	register_chrm_i(Pid),
	{reply, {ok, self()}, State};
handle_call({unregister_chrm, Pid}, _From, State) ->
	unregister_chrm_i(Pid),
	{reply, {ok, self()}, State};
handle_call({get_chrm}, _From, State) ->
	{reply, get_chrm_i(), State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', _MonitorRef, _Type, Object, _Info}, State) ->
  unregister_chrm_i(Object),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================

register_chrm_i(Pid) ->
	MonitorRef = monitor(process, Pid),
	transac_return(mnesia:transaction(fun() -> mnesia:write({?CHRM_TABLE, Pid, MonitorRef}) end)).

unregister_chrm_i(Pid) ->
	case transac_return(mnesia:transaction(fun() -> mnesia:read({?CHRM_TABLE, Pid}) end)) of
		{error, Result} ->
			{error, Result};
		_ ->
			Result = transac_return(mnesia:transaction(fun() ->  mnesia:delete({?CHRM_TABLE, Pid}) end))
	end.

get_chrm_i() ->
	{Replies, BadReplies} = chrm:chrm_counts(pids_to_nodes(chrms())),
	get_chrm_i(Replies, {nil, nil}).

get_chrm_i([{_Chrm_node, {ok, Chrm_new, Chr_count_new}} | T], {Chrm, Chr_count}) ->
	if 
		Chr_count =:= nil ->
			get_chrm_i(T, {Chrm_new, Chr_count_new});
		Chr_count > Chr_count_new ->
			get_chrm_i(T, {Chrm_new, Chr_count_new});
		true ->
			get_chrm_i(T, {Chrm, Chr_count})
	end;
get_chrm_i([], {nil, _}) ->
	{error, found_nothing};
get_chrm_i([], {Chrm, _Chr_count}) ->
	{ok, Chrm}.
	
	
init_i() ->
	init_monitors_i().

init_monitors_i() ->
	init_monitors_i(chrms()).

init_monitors_i([]) ->
	ok;
init_monitors_i([Chrm|T]) ->
	case transac_return(mnesia:transaction(fun() -> mnesia:read(?CHRM_TABLE, Chrm) end)) of
		{ok, [{chrm, Pid, nil}]} -> 
			register_chrm_i(Pid);
		{ok, [{chrm, Pid, MonitorRef}]} ->		
			catch demonitor(MonitorRef),	%ignore demonitor exceptions - if eception occures motirs is probably not local and should be dead with owner
			register_chrm_i(Pid);
		Result ->
			Result					
	end,
	init_monitors_i(T).

transac_return({atomic, Result}) ->
	{ok, Result};
transac_return({aborted, Reason}) ->
	{error, Reason}.

chrms() ->
	{atomic, Chrms} = mnesia:transaction(fun() -> mnesia:all_keys(?CHRM_TABLE) end),
	Chrms.

pids_to_nodes(Pids) ->
	pids_to_nodes(Pids, []).

pids_to_nodes([], Nodes) ->
	Nodes;
pids_to_nodes([Pid|T], Nodes) ->
	pids_to_nodes(T, [node(Pid)|Nodes] ). 