%%%-------------------------------------------------------------------
%%% @author xjurcak
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2014 6:52 PM
%%%-------------------------------------------------------------------
-module(chatroommanager_sup).
-author("xjurcak").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Limit :: integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Limit) ->
  supervisor:start_link(?MODULE, {Limit}).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init({Limit :: integer()}) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init({Limit}) ->
  MaxRestart = 1,
  MaxTime = 3600,
  {ok, {{one_for_all, MaxRestart, MaxTime},
    [{serv,
      {chatroommanager_serv, start_link, [Limit, self()]},
      permanent,
      5000, % Shutdown time
      worker,
      [chatroommanager_serv]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
