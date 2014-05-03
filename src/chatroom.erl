%%%-------------------------------------------------------------------
%%% @author xjurcak
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2014 11:21 AM
%%%-------------------------------------------------------------------
-module(chatroom).
-author("xjurcak").

-behaviour(gen_server).

%% API
-export([start_link/1, join/3, get_all_users/1, send_message/2, find_name/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { name :: term(),
                 joinedUsers = [],
                  messages = []}).

-record(user, { accesspoinpid :: pid(), name :: term(), ref}).

-include("messages.hrl").

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
  error_logger:info_report(Name),
  gen_server:start_link(?MODULE, [Name], []).

join(Pid, From, UserName) ->
  error_logger:info_report("chatroom call joingroup pid is ~p ~p ~p~n",[Pid, From, UserName]),
  gen_server:call(Pid, {joingroup, From, UserName}).

send_message(Pid, Message)->
  gen_server:call(Pid, {sendmessage, Message}).

get_all_users(Pid) ->
  gen_server:call(Pid, {getusers}).

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
init([Name]) ->
  {ok, #state{ name = Name}}.

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

handle_call({sendmessage, Message}, {Pid, _Tag}, State = #state{ joinedUsers = Users, name = Name}) ->
 error_logger:info_report("send message, from Pid: ~p~n Users: ~p~n", [Pid, Users]),
 case find_name( Users, Pid) of
   {ok, UserName} ->
     error_logger:info_report("send message, Name found: ~p~n", [UserName]),
     case catch accesspoint:receive_messages(pid_list_from_users(Users, []), #chat_message{ message = Message, timestamp = calendar:local_time(),  from = UserName}, Name) of
       Result ->
          error_logger:info_report("resend message Result, ~p~n", [Result])
     end,
     {reply, #message_ok{}, State};
  Error ->
     {reply, #message_error{reason = Error}, State}
 end;

handle_call({getusers}, _From, State = #state{ joinedUsers = Users}) ->
  {reply, #message_ok{result = Users}, State};

handle_call({joingroup, AccessPointPid, UserName}, From, State = #state{ joinedUsers = Users}) ->
  error_logger:info_report("chatroom handle_call joingroup pid is ~p~n",[From]),
  case insert_user_if_not_exist(AccessPointPid, UserName, Users) of
    nameaccesspointpidconflict ->
      {reply, #message_error{reason = nameaccesspointpidconflict}, State};
    [_H| _T] = NewUsersr ->
      {reply, #message_ok{result = joined}, State#state{ joinedUsers = NewUsersr}};
    Error ->
      {reply, #message_error{reason = unknown, reason_message = Error}, State}
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

handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{ joinedUsers = Users}) ->
  error_logger:info_report("chatroom received down msg~n"),
  {noreply, S#state{ joinedUsers = remove_user(Users, Ref, [])}};

handle_info(_Info, State) ->
  {noreply, State}.

remove_user([#user{ref = Ref} | T], Ref, Acc) ->
  remove_user(T, Ref, Acc);

remove_user([H | T] , Ref, Acc) ->
  remove_user(T, Ref, [H| Acc]);

remove_user([] , _Ref, Acc) ->
  Acc.



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

find_name([#user{accesspoinpid = Pid, name = Name} | _T], Pid) ->
  {ok, Name};

find_name([_H | T], Pid) ->
  find_name(T, Pid);

find_name([], _Pid) ->
  notjoineduser.

insert_user_if_not_exist(AccessPointPid, UserName, Users) ->
  case exist_user(UserName, AccessPointPid, Users) of
    true ->
      Users;
    false ->
      Ref = erlang:monitor(process, AccessPointPid),
      [#user{ name = UserName, accesspoinpid = AccessPointPid, ref = Ref}| Users];
    nameaccesspointpidconflict ->
      nameaccesspointpidconflict
  end.


exist_user(UserName, AccessPointPid, [#user{name = UserName, accesspoinpid = AccessPointPid} | _T]) ->
  true;

exist_user(UserName, _AccessPointPid, [#user{name = UserName} | _T]) ->
  nameaccesspointpidconflict;

exist_user(UserName, AccessPointPid, [#user{} | T]) ->
  exist_user(UserName, AccessPointPid, T);

exist_user(_UserName, _AccessPointPid, []) ->
  false.

pid_list_from_users([#user{accesspoinpid = Pid} | T], Acc) ->
  pid_list_from_users(T, [Pid| Acc]);


pid_list_from_users([], Acc) ->
  Acc.