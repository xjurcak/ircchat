%%%-------------------------------------------------------------------
%%% @author xjurcak
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Apr 2014 10:30 AM
%%%-------------------------------------------------------------------
-module(chatroommanager_serv).
-author("xjurcak").

-behaviour(gen_server).
%%-behaviour(supervisor).

-include("lookup.hrl").
-include("messages.hrl").

%% API
-export([start_link/2, get_room_multicall/2, register_lookup/0, create_room/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {limit=0,
  sup,
  chatrooms,
  refs}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Limit :: integer(), Sup :: pid()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Limit, Sup) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {Limit, Sup}, []).

-spec(register_lookup() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
register_lookup() ->
%%   TODO: register lookup
%%   net_kernel:connect_node(?LOOKUP_SERVER),
     internal_lookup:register_chrm(#netnode{name = ?SERVER, node = node()}).
%%
%%   #message_ok{}.

-spec(get_room_multicall( Node :: netnode(), Name :: term()) ->
  {ok, Node :: node()} | full ).
get_room_multicall( Nodes, Name ) ->
  gen_server:multi_call(Nodes, ?SERVER, {getroom, Name}, 5000).

-spec(create_room( Node :: netnode(), Name :: term()) ->
  {ok, Node :: node()} | full ).
create_room( #netnode{name = NodeName, node = Node}, Name ) ->
  gen_server:call({NodeName, Node}, {createroom, Name}).

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
-spec(init({Limit :: integer(), Sup :: pid()}) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init({Limit, Sup}) ->
  self() ! {register_lookup},
  self() ! {start_worker_supervisor, Sup},
  {ok, #state{limit=Limit, refs=dict:new(), chatrooms = dict:new()}}.



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

handle_call({createroom, Name}, _From, State = #state{limit=N, sup=Sup, chatrooms = CHRS, refs=R}) when N > 0 ->
  case dict:is_key(Name, CHRS) of
    true ->
      {reply, #message_error{reason = alreadyexist}, State};
    _ ->
      {ok, Pid} = supervisor:start_child(Sup, [Name]),
      Ref = erlang:monitor(process, Pid),
      {reply, #message_ok{result = Pid}, State#state{limit=N-1, chatrooms = dict:append(Name, Pid, CHRS), refs=dict:append(Ref, Name, R)}}
    end;

handle_call({createroom, _Name}, _From, S=#state{limit=N}) when N =< 0 ->
  {reply, #message_error{reason = noaloc}, S};

handle_call({getroom, Name}, _From, S=#state{chatrooms = CHR}) ->
  case dict:find(Name, CHR) of
    {ok, Pid} ->
        {reply, #message_ok{result = Pid}, S};
    _ ->
        {reply, #message_error{reason = noroom}, S}
  end;

handle_call(_, _, S) ->
  {noreply, S}.


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

handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{refs=Refs}) ->
  io:format("received down msg~n"),
  case dict:find(Ref, Refs) of
    {ok, Value} ->
      handle_down_worker(Ref, S, Value);
    _ -> %% Not our responsibility
      {noreply, S}
  end;

handle_info({register_lookup}, S = #state{}) ->
  case catch register_lookup() of
    #message_ok{} ->{noreply, S};
    Error -> {stop, Error}
  end;

handle_info({start_worker_supervisor, Sup}, S = #state{}) ->
  {ok, Pid} = supervisor:start_child(Sup, {worker_sup, {chatroommanager_worker_sup, start_link, []}, temporary, 10000, supervisor,
    [chatroommanager_worker_sup]}),
  link(Pid),
  {noreply, S#state{sup=Pid}};

handle_info(Info, State) ->
  io:format("Unknown msg: ~p~n", [Info]),
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

handle_down_worker(Ref, S = #state{limit=L, sup=_Sup, chatrooms = CHRS, refs=Refs}, Name) ->
  {noreply, S#state{limit=L+1, chatrooms = dict:erase(Name, CHRS), refs=dict:erase(Ref,Refs)}}.