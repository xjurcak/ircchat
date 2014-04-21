-module(chrm).
-behaviour(gen_server).


%% API
-export([start_link/0, chrm_counts/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("internal_lookup.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

chrm_counts(Nodes) ->
	gen_server:multi_call(Nodes, ?CHRM_LOCAL, {count_rooms}, 5000).

start_link() ->
	%% TODO: SUPERVISE
  gen_server:start_link({local, ?CHRM_LOCAL}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	case init_i() of
		{ok, _} ->
			{ok, #state{}};
		{error, Reason} ->
			{error, Reason}
	end.

handle_call({count_rooms}, _From, State) ->
	{reply, {ok, self(), count_rooms()}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================


init_i() ->
	register_i(10).

register_i(0) ->
	{error, couldnt_find_internal_lookup};
register_i(Count) ->
	case internal_lookup:exists() of 
		false ->
			timer:sleep(2000),
			register_i(Count-1);
		true ->
			internal_lookup:register_chrm(self())
	end.

count_rooms() ->
	random:uniform(100).