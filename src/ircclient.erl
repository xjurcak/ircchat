%%%-------------------------------------------------------------------
%%% @author xjurcak
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Apr 2014 2:26 PM
%%%-------------------------------------------------------------------
-module(ircclient).
-author("xjurcak").

-include("lookup.hrl").
-include("messages.hrl").

-behaviour(gen_server).
-behaviour(starter).

%% API
-export([start/0, join_the_group/1, login/1, send_message/2, receive_messages/3]).

-export([start_link/0, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, code_change/3, terminate/2]).

-record(state, { accesspoint = {}}).

start() ->
  start_link().

join_the_group(Name) ->
  gen_server:call(?MODULE, {joingroup, Name}).

login(Name) ->
  gen_server:call(?MODULE, {login, Name}).

send_message(Message, GroupName)->
  gen_server:call(?MODULE, {sendmessagegroup, Message, GroupName}).

receive_messages(Pid, Messages, Group)->
  gen_server:call(Pid, {receivemessages, Messages, Group}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE,  [] , []).

stop(Pid) ->
  gen_server:call(Pid, stop).

init([]) ->
  timer:send_interval(5000,self(),{keep_alive}),
  {ok, #state{}}.

handle_call({receivemessages, Messages, Group}, _From, State) ->
  io:format("Received Messages from group : ~p ~nmessages: ~p ~n ", [Group, Messages]),
  {reply, #message_ok{}, State};

handle_call({sendmessagegroup, _Message, _GroupName}, _From, State = #state{accesspoint = {}}) ->
  {reply, {error, "Please login first"}, State};

handle_call({sendmessagegroup, Message, GroupName}, _From, State = #state{accesspoint = Node}) ->
  case accesspoint:send_message(Node, Message, GroupName) of
    #message_ok{} ->
      {reply, {ok, messagesent}, State};
    Error->
      {reply, {error, Error}, State}
  end;

handle_call({login, Name}, _From, State = #state{accesspoint = {}}) ->

  do_func_with_accesspoint_connect(State, fun(Node) ->
    %try contact node
    net_kernel:connect(Node#netnode.node),

    case catch accesspoint:login(Node, Name) of
      #message_ok{} ->
        {reply, {ok, logged}, State#state{accesspoint = Node}};
      Error->
        {reply, {error, Error}, State#state{accesspoint = Node}}
    end
  end);

handle_call({login, Name}, From, State = #state{accesspoint = Node}) ->

    case catch accesspoint:login(Node, Name) of
      #message_ok{} ->
        {reply, {ok, logged}, State};
      #message_error{ reason = Reason} ->
        {reply, {error, Reason}, State};
      _ ->
        %accesspoint probably down try new access point
        handle_call({login, Name}, From, State#state{accesspoint = {}})
    end;

%%% OTP Callbacks
handle_call({joingroup, Name}, _From, State = #state{accesspoint = {}}) ->
  io:format("join group ~p ~n ", [Name]),
  do_func_with_accesspoint_connect(State, fun(Node) ->
    %try contact node
    io:format("join group func ~p ~p ~n ", [Name, Node]),
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

handle_call({joingroup, Name}, From, State = #state{accesspoint = Node}) ->
  io:format("join group 2 ~p ~p ~n ", [Name, Node]),
  case catch accesspoint:join_chatroom(Node, Name) of
    #message_ok{} ->
      {reply, {ok, joined}, State};
    #message_error{reason = nameaccesspointpidconflict} ->
      {reply, {error, "another client with same name already joined."}, State#state{accesspoint = Node}};
    #message_error{} = Error ->
      {reply, {error, Error}, State};
    _ ->
      %accesspoint probably down try new access point
      handle_call({joingroup, Name}, From, State#state{accesspoint = {}})
  end;


handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(Msg, _From, State) ->
  {reply, Msg, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({keep_alive}, State = #state{accesspoint = {}}) ->
  {noreply, State};
handle_info({keep_alive}, State = #state{accesspoint = Node}) ->
  case accesspoint:keep_alive(Node) of
  	#message_ok{result=alive} ->
	  {noreply, State};
	_ ->
  	  {noreply, State#state{accesspoint = {}}}
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
