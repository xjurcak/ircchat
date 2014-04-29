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
-export([get_access_point/0, start/0, join_the_group/1]).

-export([start_link/0, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, code_change/3, terminate/2]).

-record(state, { accesspoint = {}}).

start() ->
  start_link().

get_access_point() ->
  case net_adm:ping(?LOOKUP_SERVER) of
    pang ->
      {error, nolookupserver};
    pong ->
      timer:sleep(2000),
      lookup:get_access_point()
  end.

join_the_group(Name) ->
  gen_server:call(?MODULE, {joingroup, Name}).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE,  [] , []).

stop(Pid) ->
  gen_server:call(Pid, stop).

init([]) ->
  {ok, #state{}}.

%%% OTP Callbacks
handle_call({joingroup, Name}, _From, State = #state{accesspoint = {}}) ->
  case get_access_point() of
    {error, nolookupserver} = Error ->
      {reply, Error, State};
    #message_error{reason = noaccesspoint } ->
      {reply, {error, noaccesspoint}, State};
    #message_ok{result = Node} ->
      %try contact node
      net_kernel:connect(Node#netnode.node),
      case catch accesspoint:join_chatroom(Node, Name) of
        #message_ok{} ->
          {reply, {ok, joined}, #state{accesspoint = Node}};
        _ ->
          {reply, {error, 'error when conntect accesspoint'}, #state{accesspoint = Node}}
      end;
    Error -> {reply, {error, Error}, State}
  end;

handle_call({joingroup, Name}, From, State = #state{accesspoint = Node}) ->
  case catch accesspoint:join_chatroom(Node, Name) of
    #message_ok{} ->
      {reply, {ok, joined}, State};
    #message_error{ reason = Reason} ->
      {reply, {error, Reason}, State};
    _ ->
      %accesspoint probably down try new access point
      handle_call({joingroup, Name}, From, State#state{accesspoint = {}})
  end;


handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

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
