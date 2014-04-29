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

-record(state, { chatrooms, chatroomsmanagers, client_pid=nil, logged_in_as=nil, toucher=nil}).

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
  {ok, #state{ chatrooms = dict:new(), chatroomsmanagers = []}, 100000}.

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

handle_call({joinchatroom, ChatroomName}, {ClientPid, _Tag}, State = #state{chatrooms =  Chatrooms, client_pid = ClientPid}) ->
%%   Node = internal_lookup:
  login_checked(State, fun() -> 
	  case dict:find(ChatroomName, Chatrooms) of
	    {ok, Pid} ->
	      case catch chatroom:join(Pid, ClientPid) of
	        #message_ok{} ->
	          {reply, #message_ok{}, State};
	        #message_error{} = Error ->
	          {reply, Error, State};
	        _ ->
	          jointchatroom(ChatroomName, ClientPid, State#state{ chatrooms = dict:delete_element(ChatroomName, Chatrooms)})
	      end;
	    _ ->
	      jointchatroom(ChatroomName, ClientPid, State)
	  end
  end);
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({logout, LoginName, Reason}, State) ->
  {noreply, loggedout_i(LoginName, State, Reason)};
handle_cast(_Request, State) ->
  {noreply, State}.

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

jointchatroom(Name, From, State = #state{ chatroomsmanagers = []}) ->
  case internal_lookup:all_managers() of
    #message_ok{result = Managers}->
      jointchatroom(Name, Managers, From, State#state{chatroomsmanagers = Managers});
    _ ->
      {reply, #message_error{reason = internallookup}, State}
    end;

jointchatroom(Name, From, State = #state{ chatroomsmanagers = Managers}) ->
  jointchatroom(Name, Managers, From, State#state{chatroomsmanagers = Managers}).

jointchatroom(Name, Managers, From, State = #state{ chatrooms = CHRMS }) ->
  case chatroommanager:get_chat_room(Managers, Name) of
    nofind ->
      case catch get_chatroom_from_manager(Name, Managers) of
        {ok, Pid} ->
          case catch chatroom:join(Pid, From) of
            #message_ok{} ->
              {reply, #message_ok{}, State#state{chatrooms = dict:append(Name, Pid, CHRMS)}};
            #message_error{} = Error ->
              {reply, #message_error{ reason = Error}, State#state{chatrooms = dict:append(Name, Pid, CHRMS)}};
            _ ->
              {reply, #message_error{reason = jointchatroom }, State}
          end;

        _ ->
          {reply, #message_error{reason = nochatroom }, State}
      end;
    {ok, Pid} ->
      {reply, #message_ok{}, State#state{ chatrooms = dict:append(Name, Pid, CHRMS)}}
  end.

get_chatroom_from_manager(Name, [Node|T]) ->
  case catch chatroommanager:create_room(Node, Name) of
    #message_ok{ result = Pid} ->
      {ok, Pid};
    _ ->
      get_chatroom_from_manager(Name, T)
  end;

get_chatroom_from_manager(_Name, []) ->
  exit(noaccesspoint).

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
