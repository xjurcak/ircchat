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
-export([start_link/1, keep_alive/1, login/2, connect/1, join_chatroom/2, send_message/3, send_message_user/3, receive_messages/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(EXPIRATION_TIME, 30).
-include("messages.hrl").

-record(state, { name, chatrooms, username, chatrooms_ref, client_pid=nil, client_exp=nil, logged_in_as=nil}).
%%%===================================================================
%%% API
%%%===================================================================

start_link(Name) ->
  error_logger:info_report(Name),
  gen_server:start_link({local, Name}, ?MODULE, [Name], []).

receive_messages([Pid| T], Message, Group) ->
  error_logger:info_report("receive_massages multicall pidlist ~p~n", [Pid]),
  case catch gen_server:cast(Pid, {receivemessages, Message, Group})of
    _ ->
      receive_messages(T, Message, Group)
  end;

receive_messages([], _Messages, _Group) ->
   #message_ok{}.

join_chatroom(#netnode{name = Name, node = Node}, ChatroomName) ->
  gen_server:call({Name, Node}, {joinchatroom, ChatroomName}).

login(#netnode{name = Name, node = Node}, LoginName) ->
  gen_server:call({Name, Node}, {login, LoginName}).

send_message_user(#netnode{name = Name, node = Node}, Message, UserName)->
  gen_server:call({Name, Node}, {sendmessageuser, Message, UserName}).
send_message(#netnode{name = Name, node = Node}, Message, GroupName)->
  gen_server:call({Name, Node}, {sendmessagegroup, Message, GroupName}).

connect(#netnode{name = Name, node = Node}) ->
  gen_server:call({Name, Node}, {connect}).

keep_alive(#netnode{name = Name, node = Node}) ->
  gen_server:call({Name, Node}, {keep_alive}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name]) ->
  timer:send_interval(5000,self(),{check_expiration}),
  timer:send_interval(5000,self(),{touch}),
  {ok, #state{name=Name, chatrooms = dict:new(), chatrooms_ref = dict:new()}, 100000}.

handle_call({sendmessageuser, Message, UserName}, _From, State = #state{client_pid = ClientPid}) ->
  case login_server:get_listener(UserName) of
    #message_ok{result=not_logged}->
	    {reply, #message_error{reason = user_is_not_logged_in}, State};
    #message_ok{result=Listener} ->
		NewMessage = #chat_message{message=Message,timestamp=calendar:local_time(),from=State#state.logged_in_as},
		accesspoint:receive_messages([Listener], NewMessage, users_to_group(UserName, State#state.logged_in_as)),
		client_receive_messages_i(ClientPid, NewMessage, users_to_group(UserName, State#state.logged_in_as), State),
		{reply, #message_ok{result=sent}, State};
	_->
	    {reply, #message_error{reason = something_goes_wrong}, State}
  end;
handle_call({sendmessagegroup, Message, GroupName}, _From, State = #state{chatrooms = Chatrooms}) ->
  case dict:find(GroupName, Chatrooms) of
    {ok, [Pid | _T]} ->
      error_logger:info_report("joint chatroom already joined"),
      case catch chatroom:send_message(Pid, Message) of
        #message_ok{} ->
          {reply, #message_ok{result=sent}, State};
        #message_error{reason = Reason, reason_message = ReasonMessage} ->
          {reply, #message_error{reason = Reason, reason_message = ReasonMessage}, State};
        Error ->
          {reply, #message_error{reason = Error}, State}
      end;
    _ ->
      error_logger:info_report("access point not joined to room"),
      {reply, #message_error{reason=notjoined}, State}
  end;

handle_call({connect}, {ClientPid, _Tag}, State = #state{client_pid = nil}) ->
	{reply, #message_ok{result={connected}}, State#state{client_pid = ClientPid, client_exp=timestamp(now())+?EXPIRATION_TIME}};
handle_call({connect}, {ClientPid, _Tag}, State = #state{client_pid = ClientPid}) ->
	{reply, #message_error{reason={already_connected}}, State};
handle_call({connect}, _From, State) ->
	{reply, #message_error{reason={already_engaged}}, State};

handle_call({login, LoginName}, {ClientPid, _Tag}, State = #state{logged_in_as=LoggedInAs, client_pid = ClientPid}) ->
%%   Node = internal_lookup:
	case LoggedInAs of
		nil ->
			case login_server:login(LoginName, {State#state.name, node()}) of
				#message_ok{result=logged} = Result ->
					NewState = State#state{logged_in_as=LoginName},
					{reply, Result, NewState};
				#message_ok{result=Result} -> 
					{reply, #message_error{reason={Result, LoggedInAs}}, State}
			end;	
		_ ->
			{reply, #message_error{reason={already_logged_in, LoggedInAs}}, State}
	end;

handle_call({joinchatroom, ChatroomName}, {ClientPid, _Tag}, State = #state{chatrooms =  Chatrooms, client_pid = ClientPid, logged_in_as=LoginName, chatrooms_ref = R}) ->
%%   Node = internal_lookup:
  login_checked(State, fun() -> 
	  case dict:find(ChatroomName, Chatrooms) of
    {ok, _Pid} ->
      error_logger:info_report("joint chatroom already joined"),
      {reply, #message_ok{ result = allreadyinchatroom}, State};
    _ ->
      error_logger:info_report("joint chatroom find room or create"),
      case catch internal_lookup:find_room_or_create(ChatroomName) of
        #message_ok{result = Pid} ->
          error_logger:info_report("joint chatroom join chatroom"),
          case catch chatroom:join(Pid, self(), LoginName) of
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
  end
  end);

handle_call({keep_alive}, _From, State = #state{client_exp=nil}) ->
  {reply, #message_ok{result=disconnected}, State};
handle_call({keep_alive}, _From, State) ->
  {reply, #message_ok{result=alive}, State#state{client_exp=timestamp(now())+?EXPIRATION_TIME}};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({receivemessages, Messages, Group}, State = #state{client_pid = ClientPid}) ->
  client_receive_messages_i(ClientPid, Messages, Group, State);

handle_cast({logout, LoginName, Reason}, State) ->
  {noreply, loggedout_i(LoginName, State, Reason)};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{chatrooms =  Chatrooms, chatrooms_ref = R}) ->
  error_logger:info_report("received down msg~n"),


  case dict:find(Ref, R) of
    {ok, [Name | _T]} ->
      {noreply, S#state{chatrooms = dict:erase(Name, Chatrooms) , chatrooms_ref = dict:erase(Ref, R)}};
    _ -> %% Not our responsibility
      {noreply, S}
  end;

handle_info(timeout, _State) ->
  {stop, normal, #state{}};
handle_info({'DOWN', _MonitorRef, _Type, Object, _Info}, State = #state{client_pid = Object}) ->
  {stop,client_down,State};

handle_info({check_expiration}, State = #state{client_exp=nil}) ->
  {noreply, State};
handle_info({check_expiration}, State = #state{client_exp=ClientExp})->
 Now =  timestamp(now()),
 error_logger:info_report("~p ~p~n",[ClientExp,Now]),
 if 
    ClientExp < Now -> 
      {stop, normal, clear_client(State)};
	true ->
	  {noreply, State}
 end;

handle_info({touch}, State = #state{logged_in_as=nil}) ->
  {noreply, State};
handle_info({touch}, State = #state{logged_in_as=LoggedInAs}) ->
  touch(LoggedInAs),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

clear_client(State) ->
	State#state{client_pid = nil, client_exp = nil, logged_in_as = nil}.

login_checked(State, Func) ->
	case is_logged_in(State) of
	false ->
	  {reply, #message_error{reason = not_logged_in}, State};
	_ ->
	  Func()
	end.

is_logged_in(#state{logged_in_as=nil}) ->
	false;
is_logged_in(#state{logged_in_as=_}) ->
	true.

	 
client_receive_messages_i(ClientPid, Messages, Group, State) ->
	client:receive_messages(ClientPid, Messages, Group),
	{noreply, State}.
	
loggedout_i(LoginName, State, _Reason) ->
	error_logger:info_report("expired ~p~n", LoginName),
	State#state{logged_in_as = nil}.

touch(LoginName) ->
	login_server:touch(LoginName).

timestamp({Megasecs, Secs, _}) ->
     (Megasecs * 1000000) + Secs.

users_to_group(N1, N2) ->
	list_to_atom(lists:concat(lists:sort([N1, N2]))).