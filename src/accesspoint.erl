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
-export([start_link/1, login/2, connect/1, join_chatroom/2]).
-export([touch_loop/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-include("messages.hrl").

-record(state, { chatrooms, username, chatrooms_ref, client_pid=nil, logged_in_as=nil, toucher=nil}).
%%%===================================================================
%%% API
%%%===================================================================

start_link(Name) ->
  io:format(Name),
  gen_server:start_link({local, Name}, ?MODULE, [], []).

join_chatroom(#netnode{name = Name, node = Node}, ChatroomName) ->
  gen_server:call({Name, Node}, {joinchatroom, ChatroomName}).
login(#netnode{name = Name, node = Node}, LoginName) ->
  gen_server:call({Name, Node}, {login, LoginName}).
connect(#netnode{name = Name, node = Node}) ->
  gen_server:call({Name, Node}, {connect}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{ chatrooms = dict:new(), chatrooms_ref = dict:new()}, 100000}.

handle_call({connect}, {ClientPid, _Tag}, State = #state{client_pid = nil}) ->
	monitor(process, ClientPid),
	{reply, #message_ok{result={connected}}, State#state{client_pid = ClientPid}};
handle_call({connect}, {ClientPid, _Tag}, State = #state{client_pid = ClientPid}) ->
	{reply, #message_error{reason={already_connected}}, State};
handle_call({connect}, _From, State) ->
	{reply, #message_error{reason={already_engaged}}, State};

handle_call({login, LoginName}, {ClientPid, _Tag}, State = #state{logged_in_as=LoggedInAs, client_pid = ClientPid}) ->
%%   Node = internal_lookup:
	case LoggedInAs of
		nil ->
			case login_server:login(LoginName, self()) of
				#message_ok{result=logged} = Result ->
					NewState = State#state{logged_in_as=LoginName,toucher=spawn_link(?MODULE, touch_loop, [LoginName])},
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
      io:format("joint chatroom already joined"),
      {reply, #message_ok{ result = allreadyinchatroom}, State};
    _ ->
      io:format("joint chatroom find room or create"),
      case catch internal_lookup:find_room_or_create(ChatroomName) of
        #message_ok{result = Pid} ->
          io:format("joint chatroom join chatroom"),
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
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({logout, LoginName, Reason}, State) ->
  {noreply, loggedout_i(LoginName, State, Reason)};
handle_cast(_Request, State) ->
  {noreply, State}.

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

handle_info({'DOWN', _MonitorRef, _Type, Object, _Info}, State = #state{client_pid = Object}) ->
  {stop,client_down,State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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

loggedout_i(LoginName, State, _Reason) ->
	io:format("expired ~p~n", LoginName),
	exit(State#state.toucher, normal),
	State#state{logged_in_as = nil}.

touch_loop(LoginName) ->
	timer:sleep(5000),
	case login_server:touch(LoginName) of
		#message_ok{result=touched} ->
			 touch_loop(LoginName)
	end.
