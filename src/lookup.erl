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

%% API
-export([start_link/0, get_access_point/0, register_access_point_manager/1, all_managers/0]).

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

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% return access point
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_access_point() ->
  {ok, AccessPoint :: netnode()} | {error, Reason :: term()}).
get_access_point() ->
  %gen_server:call({global, ?LOOKUP_SERVER_GLOBAL}, {accesspoint}).
  gen_server:call(?LOOKUP_SERVER_GLOBAL, {accesspoint}).

%%--------------------------------------------------------------------
%% @doc
%% register access point
%%
%% @end
%%--------------------------------------------------------------------
-spec(register_access_point_manager( AccessPointManager :: netnode() ) ->
  {ok} | {error, Reason :: term()}).
register_access_point_manager(AccessPointManager) ->
  gen_server:call(?LOOKUP_SERVER_GLOBAL, {accesspointmanager, AccessPointManager}).

all_managers() ->
  gen_server:call(?LOOKUP_SERVER_GLOBAL, {all}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, Table} = dets:open_file(?MANAGERS_TABLE, [{repair, force}]),
  Managers = managers_from_table(Table),
  dets:close(Table),
  {ok, #state{ managers = Managers}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).


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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
insert(Managers, Node) ->
  case sets:is_element(Node, Managers) of
    false ->
      dets:open_file(?MANAGERS_TABLE, [{repair, force}]),
      dets:insert(?MANAGERS_TABLE, {key_from_node(Node), Node } ),
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
  managers_from_table(Table, dets:first(Table), sets:new()).

managers_from_table(_Table, '$end_of_table', Set) ->
  Set;

managers_from_table(Table, Key, Set) ->
  List = dets:lookup(Table, Key),
  managers_from_table(Table, dets:next(Table, Key), table_entry_to_set(List, Set)).

table_entry_to_set([{_Key, #netnode{} = Node}| T], Set) ->
  table_entry_to_set(T, sets:add_element(Node, Set));

table_entry_to_set([], Set) ->
  Set.


key_from_node(#netnode{name = Name, node = Node}) ->
  lists:concat([Name, Node]).
