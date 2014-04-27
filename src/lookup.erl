%%%-------------------------------------------------------------------
%%% @author xjurcak
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Apr 2014 2:04 PM
%%%-------------------------------------------------------------------
-module(lookup).
-author("xjurcak").

-behaviour(gen_server).
-behaviour(backable).

%% API
-export([start/0, start/1, get_access_point/0, register_access_point_manager/1, all_managers/0]).

-export([start_up/0, global_name/0, on_before_backup/1, on_master_start/0, on_slave_start/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(MANAGERS_TABLE, lookup).

-include("lookup.hrl").
-include("messages.hrl").

-record(state, { managers = sets:new() :: set()}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
	 backable:backup(node(),  ?MODULE).

start(Node) ->
	start(Node, 10).
start(_Node, 0) ->
	{error, couldnt_connect_to_node};
start(Node, Count) ->
	case net_adm:ping(Node) of
		pong ->
			timer:sleep(2000), %% wait till synced data
			start();
		pang ->
			timer:sleep(5000),
			start(Node, Count-1)
	end.


get_access_point() ->
  %gen_server:call({global, ?LOOKUP_SERVER_GLOBAL}, {accesspoint}).
  gen_server:call({global,?LOOKUP_SERVER_GLOBAL}, {accesspoint}).

register_access_point_manager(AccessPointManager) ->
  gen_server:call({global, ?LOOKUP_SERVER_GLOBAL}, {accesspointmanager, AccessPointManager}).

all_managers() ->
  gen_server:call({global,?LOOKUP_SERVER_GLOBAL}, {all}).

%%%===================================================================
%%% backable callbacks
%%%===================================================================

start_up() ->
	Ret = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
	case Ret of
		{ok, Pid} -> unlink(Pid)
	end,
	Ret.

on_before_backup(_Node) ->
	io:format("initializing node '~p' ~n", [node()]),
	mnesia:delete_schema([node()]),
	case global:whereis_name(global_name()) of
		undefined ->
			on_master_start();
		Pid ->
			on_slave_start(node(Pid))
	end.

on_master_start() ->
	io:format("initializing as MASTER node ~p~n", [node()]),	
	mnesia:start(),
    mnesia:create_schema([node()]),
    mnesia:create_table(?MANAGERS_TABLE, []).
	 

on_slave_start(MasterNode) ->
	io:format("initializing as SLAVE node ~p~n", [node()]),	
	mnesia:start(),
    mnesia:change_config(extra_db_nodes, [MasterNode]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    Tabs = mnesia:system_info(tables) -- [schema],
    [mnesia:add_table_copy(Tab,node(), disc_copies) || Tab <- Tabs].

global_name() ->
	?LOOKUP_SERVER_GLOBAL.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  Managers = managers_from_table(?MANAGERS_TABLE),
  {ok, #state{ managers = Managers}}.

handle_call({accesspoint}, _From, #state{managers = Managers} = State) ->
  case catch get_accesspoint_from_manager(sets:to_list(Managers)) of
    #netnode{} = NetNode ->
      {reply, #message_ok{result = NetNode}, State};
    _ ->
      {reply, #message_error{reason = noaccesspoint }, State}
  end;


handle_call({accesspointmanager, Node}, _From, #state{ managers = Managers }) ->
  NewSet = insert(Managers, Node),
  {reply, #message_ok{result = sets:to_list(NewSet)}, #state{managers = NewSet}};

handle_call({all}, _From, State = #state{ managers = Managers }) ->
  {reply, #message_ok{result = sets:to_list(Managers)}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
insert(Managers, Node) ->
  case sets:is_element(Node, Managers) of
    false ->
	  transac_return(mnesia:transaction(fun() -> mnesia:write({?MANAGERS_TABLE, key_from_node(Node), Node}) end)),
      sets:add_element(Node, Managers);
    _->
      io:format("Manager already in qeue"),
      Managers
  end.


get_accesspoint_from_manager([Node|T]) ->
  case catch accesspointmanager_serv:get_access_point(Node) of
    #message_ok{ result = Result} ->
      Result;
    _ ->
      get_accesspoint_from_manager(T)
  end;



get_accesspoint_from_manager([]) ->
  exit(noaccesspoint).

managers_from_table(Table) ->
  First = transac_return(mnesia:transaction(fun() ->  mnesia:first(?MANAGERS_TABLE) end)),
  managers_from_table(Table, First, sets:new()).

managers_from_table(_Table, '$end_of_table', Set) ->
  Set;

managers_from_table(Table, Key, Set) ->
	case transac_return(mnesia:transaction(fun() -> mnesia:read(Table, Key) end)) of
		{ok, [{?MANAGERS_TABLE, _, List}]} ->
			managers_from_table(Table, dets:next(Table, Key), table_entry_to_set(List, Set));
		_ ->
			  managers_from_table(Table, '$end_of_table', Set)
	end.

table_entry_to_set([{_Key, #netnode{} = Node}| T], Set) ->
  table_entry_to_set(T, sets:add_element(Node, Set));

table_entry_to_set([], Set) ->
  Set.

transac_return({atomic, Result}) ->
	{ok, Result};
transac_return({aborted, Reason}) ->
	{error, Reason}.

key_from_node(#netnode{name = Name, node = Node}) ->
  lists:concat([Name, Node]).
