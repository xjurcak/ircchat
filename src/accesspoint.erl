%%%-------------------------------------------------------------------
%%% @author xjurcak
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2014 11:21 AM
%%%-------------------------------------------------------------------
-module(accesspoint).
-author("xjurcak").

-behaviour(gen_server).

%% API
-export([start_link/1, join_chatroom/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-include("messages.hrl").

-record(state, { chatrooms, username, chatrooms_ref }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Name :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name) ->
  io:format(Name),
  gen_server:start_link({local, Name}, ?MODULE, [], []).

join_chatroom(#netnode{name = Name, node = Node}, ChatroomName) ->
  gen_server:call({Name, Node}, {joinchatroom, ChatroomName}).


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
  {ok, #state{ chatrooms = dict:new(), chatrooms_ref = dict:new()}, 100000}.

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

handle_call({joinchatroom, ChatroomName}, From, State = #state{chatrooms =  Chatrooms, username = UserName, chatrooms_ref = R}) ->
%%   Node = internal_lookup:
  case dict:find(ChatroomName, Chatrooms) of
    {ok, _Pid} ->
      {reply, #message_ok{ result = allreadyinchatroom}, State};
    _ ->
      case catch internal_lookup:find_room_or_create(ChatroomName) of
        #message_ok{result = Pid} ->
          case catch chatroom:join(Pid, From, UserName) of
            #message_ok{} ->
              Ref = erlang:monitor(process, Pid),
              {reply, #message_ok{ result = Pid}, State#state{ chatrooms = dict:append(ChatroomName, Pid, Chatrooms), chatrooms_ref = dict:append(Ref, ChatroomName, R)}};
            #message_error{} = Error ->
              {reply, Error, State};
            Error ->
              {reply, #message_error{reason = unknown ,reason_message = Error}, State}
          end;

        #message_error{} = Error ->
          {reply, Error, State};
        _ = Error ->
          {reply, #message_error{reason = error, reason_message = Error}, State}
      end
  end;

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

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

handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{chatrooms =  Chatrooms, chatrooms_ref = R}) ->
  io:format("received down msg~n"),


  case dict:find(Ref, R) of
    {ok, [Name | _T]} ->
      {noreply, S#state{chatrooms = dict:erase(Name, Chatrooms) , chatrooms_ref = dict:erase(Ref, R)}};
    _ -> %% Not our responsibility
      {noreply, S}
  end;

handle_info(timeout, _State) ->
  {stop, normal, #state{}};

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