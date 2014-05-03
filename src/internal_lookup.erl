-module(internal_lookup).
-behaviour(gen_server).
-behaviour(starter).


%% API
-export([start/0, start_slave/0, start_link/0, all_managers/0, find_room_or_create/1]).
-export([register_chrm/1, unregister_chrm/1, get_chrm/1, exists/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("internal_lookup.hrl").
-include("messages.hrl").

-record(state, { managers = sets:new() :: set(), chatrooms = dict:new()}).

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

register_chrm(#netnode{} = ChatroomManager) ->
	gen_server:call({global, ?SERVER_GLOBAL}, {register_chrm, ChatroomManager}).

unregister_chrm(#netnode{} = ChatroomManager) ->
	gen_server:call({global, ?SERVER_GLOBAL}, {unregister_chrm, ChatroomManager}).

get_chrm(Name) ->
	gen_server:call({global, ?SERVER_GLOBAL}, {get_chrm, Name}).

all_managers() ->
  gen_server:call({global, ?SERVER_GLOBAL}, {all}).

find_room_or_create(ChatroomName) ->
  gen_server:call({global, ?SERVER_GLOBAL}, {getroom, ChatroomName}).

%% starter behaviour
start() ->
	internal_lookup_supervisor:start().
	
start_slave() ->
	internal_lookup_supervisor:start_slave().

% should not be used for server start!!!
start_link() ->
  gen_server:start_link({global, ?SERVER_GLOBAL}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  Managers = managers_from_table(?MANAGERS_TABLE),
  {ok, #state{ managers = Managers, chatrooms = dict:new()}}.

handle_call({getroom, ChatroomName}, _From, State = #state{chatrooms =  Chatrooms, managers = Managers}) ->
%%   Node = internal_lookup:
  case dict:find(ChatroomName, Chatrooms) of
    {ok, [Pid| _T]} ->
      {reply, #message_ok{result = Pid}, State};
    _ ->
      case create_if_not_exist(ChatroomName, sets:to_list(Managers)) of
        {ok, Pid} ->
          {reply, #message_ok{result = Pid}, State#state{ chatrooms = dict:append(ChatroomName, Pid, Chatrooms)}};
        _ ->
          {reply, #message_error{reason = nochatroom}, State}
      end
  end;

handle_call({hello}, _From, State) ->
  {reply, {ok, self()}, State};

handle_call({register_chrm, #netnode{} = Node}, _From, #state{managers = Managers}) ->
	NewManagers = insert(Node, Managers),
	{reply, #message_ok{}, #state{managers = NewManagers}};

handle_call({unregister_chrm, #netnode{} = Node}, _From, #state{managers = Managers}) ->
   NewManagers = delete(Node, Managers),
	{reply, #message_ok{}, #state{managers = NewManagers}};

handle_call({get_chrm, _Name}, _From, State) ->
	{reply, get_chrm_i(), State};

handle_call({all}, _From, State = #state{ managers = Managers }) ->
  {reply, #message_ok{result = sets:to_list(Managers)}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info}, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================

insert( Node, Managers) ->
  case sets:is_element(Node, Managers) of
    false ->
      transac_return(mnesia:transaction(fun() -> mnesia:write({?MANAGERS_TABLE, key_from_node(Node), Node}) end)),
      sets:add_element(Node, Managers);
    _->
      error_logger:info_report("Manager already in qeue"),
      Managers
  end.

delete(Node, Managers) ->
  Result = sets:del_element(Node, Managers),
	case transac_return(mnesia:transaction(fun() -> mnesia:read({?CHRM_TABLE, key_from_node(Node)}) end)) of
		{error, Result} ->
			ok;
		_ ->
			transac_return(mnesia:transaction(fun() ->  mnesia:delete({?CHRM_TABLE, key_from_node(Node)}) end))
	end,
  Result.

get_chrm_i() ->
	{Replies, _BadReplies} = chrm:chrm_counts(pids_to_nodes(chrms())),
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

chrms() ->
	{atomic, Chrms} = mnesia:transaction(fun() -> mnesia:all_keys(?CHRM_TABLE) end),
	Chrms.

pids_to_nodes(Pids) ->
	pids_to_nodes(Pids, []).

pids_to_nodes([], Nodes) ->
	Nodes;
pids_to_nodes([Pid|T], Nodes) ->
	pids_to_nodes(T, [node(Pid)|Nodes] ).

key_from_node(#netnode{name = Name, node = Node}) ->
  lists:concat([Name, Node]).

create_if_not_exist(Name, Managers) ->
  case chatroommanager:get_chat_room(Managers, Name) of
    nofind ->
      case chatroommanager:create_room(Managers, Name) of
        #message_ok{ result = Pid} ->
          {ok, Pid};
        _ ->
          #message_error{reason = nochatroom}
      end;
    {ok, Pid} ->
      {ok, Pid}
  end.
