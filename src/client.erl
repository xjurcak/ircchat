-module(client).

-include("lookup.hrl").
-include("messages.hrl").

-behaviour(gen_server).
-behaviour(starter).

%% API
-export([start/0, j2g/1, login/1, s2u/2, s2g/2, msgs_u/1, msgs_g/1, receive_messages/3]).

-export([start_link/0, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, code_change/3, terminate/2]).

-record(state, { login=nil, accesspoint = {}, msgs}).

start() ->
  start_link().

j2g(Name) ->
  gen_server:call(?MODULE, {joingroup, Name}).

login(Name) ->
  gen_server:call(?MODULE, {login, Name}).

s2u(Message, UserName)->
  gen_server:call(?MODULE, {sendmessageuser, Message, UserName}).
s2g(Message, GroupName)->
  gen_server:call(?MODULE, {sendmessagegroup, Message, GroupName}).
  
msgs_u(UserName) ->
	gen_server:call(?MODULE, {show_msgs_u, UserName}).
msgs_g(Group) ->
	gen_server:call(?MODULE, {show_msgs, Group}).

receive_messages(Pid, Messages, Group)->
  gen_server:cast(Pid, {receivemessages, Messages, Group}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE,  [] , []).

stop(Pid) ->
  gen_server:call(Pid, stop).

init([]) ->
  timer:send_interval(5000,self(),{keep_alive}),
  {ok, #state{msgs=dict:new()}}.
 

handle_call({show_msgs_u, UserName}, _From, State) ->
	connected_and_logged(State, fun() ->
		{reply, print_msgs(State, users_to_group(State#state.login, UserName)), State}
	end);
handle_call({show_msgs, Group}, _From, State) ->
	connected_and_logged(State, fun() ->
		{reply, print_msgs(State, Group), State}
	end);
	

handle_call({sendmessageuser, Message, UserName}, _From, State) ->
	connected_and_logged(State, fun() ->
		send_message_user_i(Message, UserName,  State)
	end);
handle_call({sendmessagegroup, Message, GroupName}, _From, State) ->
	connected_and_logged(State, fun() ->
		send_message_group_i(Message, GroupName,  State)
	end);

handle_call({login, Name}, _From, State = #state{accesspoint = {}}) ->

  do_func_with_accesspoint_connect(State, fun(Node) ->
    %try contact node
    net_kernel:connect(Node#netnode.node),

    case catch accesspoint:login(Node, Name) of
      #message_ok{} ->
        {reply, {ok, logged}, State#state{accesspoint = Node, login=Name}};
      Error->
        {reply, {error, Error}, State#state{accesspoint = Node}}
    end
  end);

handle_call({login, Name}, From, State = #state{accesspoint = Node}) ->

    case catch accesspoint:login(Node, Name) of
      #message_ok{} ->
        {reply, {ok, logged}, State#state{login=Name}};
      #message_error{ reason = Reason} ->
        {reply, {error, Reason}, State};
      _ ->
        %accesspoint probably down try new access point
        handle_call({login, Name}, From, State#state{accesspoint = {}})
    end;

%%% OTP Callbacks
handle_call({joingroup, Name}, _From, State) ->
	connected_and_logged(State, fun() ->
		join_group_i(Name, State)
	end);


handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(Msg, _From, State) ->
  {reply, Msg, State}.

handle_cast({receivemessages, Message, Group}, State) -> 
  NewMsgs = case dict:is_key(Group, State#state.msgs) of
    false ->
	  dict:store(Group, [Message], State#state.msgs);
	true ->
	  Msgs = dict:fetch(Group, State#state.msgs),
      dict:store(Group, lists:append(Msgs, [Message]), State#state.msgs)
  end,  
  print_msg(Group, Message),
  {noreply, State#state{msgs=NewMsgs}};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({keep_alive}, State = #state{accesspoint = {}}) ->
  {noreply, State};
handle_info({keep_alive}, State = #state{accesspoint = Node}) ->
  case accesspoint:keep_alive(Node) of
   #message_ok{result=alive} ->
	  {noreply, State};
	_ ->
  	  {noreply, State#state{login=nil,accesspoint = {}}}
  end;
handle_info(_, State) ->
  {noreply, State}.
%% We cannot use handle_info below: if that ever happens,
%% we cancel the timeouts (Delay) and basically zombify
%% the entire process. It's better to crash in this case.
%% handle_info(_Msg, State) ->
%%    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) -> ok.

send_message_user_i(_Message, _UserName, State = #state{accesspoint = {}}) ->
  {reply, {error, "Please login first"}, State};
send_message_user_i(Message, UserName, State = #state{accesspoint = Node}) ->
  case accesspoint:send_message_user(Node, Message, UserName) of
    #message_ok{} ->
      {reply, {ok, messagesent}, State};
    Error->
      {reply, {error, Error}, State}
  end.

send_message_group_i(_Message, _GroupName, State = #state{accesspoint = {}}) ->
  {reply, {error, "Please login first"}, State};
send_message_group_i(Message, GroupName, State = #state{accesspoint = Node}) ->
  case accesspoint:send_message(Node, Message, GroupName) of
    #message_ok{} ->
      {reply, {ok, messagesent}, State};
    Error->
      {reply, {error, Error}, State}
  end.

join_group_i(Name, State = #state{accesspoint = {}}) ->
  io:format("join group ~p ~n ", [Name]),
  do_func_with_accesspoint_connect(State, fun(Node) ->
    %try contact node
    error_logger:info_report("join group func ~p ~p ~n ", [Name, Node]),
    net_kernel:connect(Node#netnode.node),

      case catch accesspoint:join_chatroom(Node, Name) of
        #message_ok{} ->
          {reply, {ok, joined}, State#state{accesspoint = Node}};
        #message_error{reason = nameaccesspointpidconflict} ->
          {reply, {error, "another client with same name already joined."}, State#state{accesspoint = Node}};
        Error->
          {reply, {error, Error}, State#state{accesspoint = Node}}
      end
    end);
join_group_i(Name, State = #state{accesspoint = Node}) ->
  io:format("join group 2 ~p ~p ~n ", [Name, Node]),
  case catch accesspoint:join_chatroom(Node, Name) of
    #message_ok{} ->
      {reply, {ok, joined}, State};
    #message_error{reason = nameaccesspointpidconflict} ->
      {reply, {error, "another client with same name already joined."}, State#state{accesspoint = Node}};
    #message_error{} = Error ->
      {reply, {error, Error}, State};
    _ ->
      {reply, {error, ap_probably_down}, State}
  end.

do_func_with_accesspoint_connect(State, Func) ->
  case catch lookup:get_access_point() of
    #message_error{reason = noaccesspoint } ->
      {reply, {error, noaccesspoint}, State};
    #message_ok{result = Node} ->
      %try contact node
      case accesspoint:connect(Node) of
        #message_ok{} ->
            Func(Node);
        Error -> {reply, {accesspointconnecterror, Error}, State}
       end;
    Error -> {reply, {error, Error}, State}
  end.

connected_and_logged(State, Func) ->
	if 
		State#state.accesspoint =:= {} ->
			#message_error{reason=not_connected};
		State#state.login =:= nil ->
			#message_error{reason=not_loggedin};
		true ->
			Func()
	end.

print_msgs(State, Group) ->
	case dict:is_key(Group, State#state.msgs) of
		true ->
			print_msgs(dict:fetch(Group, State#state.msgs));
		false ->
			print_msgs([])
	end.

print_msgs([]) ->
  	ok;
print_msgs([Msg | T]) ->
	print_msg(Msg),
	print_msgs(T).

print_msg(Group, Msg) ->
	io:format("~p > [~p]~p: ~p~n", [Group, Msg#chat_message.timestamp, Msg#chat_message.from, Msg#chat_message.message]).
print_msg(Msg) ->
	io:format("[~p]~p: ~p~n", [Msg#chat_message.timestamp, Msg#chat_message.from, Msg#chat_message.message]).

users_to_group(N1, N2) ->
	list_to_atom(lists:concat(lists:sort([N1, N2]))).