-module(login_server).

-behaviour(gen_server).
-behaviour(backable).
-behaviour(starter).

%% API
-export([start/0, login/1, logout/1, touch/1, is_logged/1]).

-export([start_up/0, global_name/0, on_before_backup/1, on_master_start/0, on_slave_start/1]).

-export([clean_loop/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(LOGINS_TABLE, login).

-include("login_server.hrl").
-include("messages.hrl").

-record(state, {}).
-record(login, {name, expiration}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
	backable:backup(node(),  ?MODULE).

login(LoginName) ->
  gen_server:call({global, ?LOGIN_SERVER_GLOBAL}, {login, LoginName}).

logout(LoginName) ->
  gen_server:call({global, ?LOGIN_SERVER_GLOBAL}, {logout, LoginName}).

touch(LoginName) ->
  gen_server:call({global, ?LOGIN_SERVER_GLOBAL}, {touch, LoginName}).

is_logged(LoginName) ->
  gen_server:call({global, ?LOGIN_SERVER_GLOBAL}, {is_logged, LoginName}).

%%%===================================================================
%%% backable callbacks
%%%===================================================================

start_up() ->
	Ret = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
	case Ret of
		{ok, Pid} -> unlink(Pid)
	end,
	Ret.

on_before_backup(_Node) ->
	io:format("initializing node '~p' ~n", [node()]),
	mnesia:delete_schema([node()]),
	case global:whereis_name(global_name()) of
		undefined ->
			on_master_start();
		Pid ->
			on_slave_start(node(Pid))
	end.

on_master_start() ->
	io:format("initializing as MASTER node ~p~n", [node()]),	
	mnesia:start(),
    mnesia:create_schema([node()]),
    mnesia:create_table(?LOGINS_TABLE, []).
	 

on_slave_start(MasterNode) ->
	io:format("initializing as SLAVE node ~p~n", [node()]),	
	mnesia:start(),
    mnesia:change_config(extra_db_nodes, [MasterNode]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    Tabs = mnesia:system_info(tables) -- [schema],
    [mnesia:add_table_copy(Tab,node(), disc_copies) || Tab <- Tabs].

global_name() ->
	?LOGIN_SERVER_GLOBAL.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	spawn_link(?MODULE, clean_loop, []),
  	{ok, #state{}}.

handle_call({is_logged, LoginName}, _From, State = #state{}) ->
  {reply, #message_ok{result=is_logged_i(LoginName)}, State};
handle_call({touch, LoginName}, _From, State = #state{}) ->
  {reply, #message_ok{result=touch_i(LoginName)}, State};
handle_call({logout, LoginName}, _From, State = #state{}) ->
  {reply, #message_ok{result=logout_i(LoginName)}, State};
handle_call({login, LoginName}, _From, State = #state{}) ->
  {reply, #message_ok{result=login_i(LoginName)}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
is_logged_i(LoginName) ->
	transac_return(mnesia:transaction(fun() ->
	  case mnesia:read(?LOGINS_TABLE, LoginName) of
	    [] ->
		  not_logged;
	    _->
		  logged
	  end
	end)).

touch_i(LoginName) ->
	Login = createLogin(LoginName),
	transac_return(mnesia:transaction(fun() ->
	  case mnesia:read(?LOGINS_TABLE, LoginName) of
	    [] ->
		  not_logged;
	    _->
		  mnesia:write(?LOGINS_TABLE, Login, write),
		  touched
	  end
	end)).

login_i(LoginName) ->
	Login = createLogin(LoginName),
	transac_return(mnesia:transaction(fun() ->
	  case mnesia:read(?LOGINS_TABLE, LoginName) of
	    [] ->
		  Result = mnesia:write(?LOGINS_TABLE, Login, write),
		  Result;
	    _->
	      already_logged
	  end
	end)).

logout_i(Login) ->
	mnesia:dirty_delete(?LOGINS_TABLE, Login#login.name).

clean_loop() ->
	clean(),
	timer:sleep(5000),
	clean_loop().

clean() ->
	transac_return(mnesia:transaction(fun() ->
		MatchHead = #login{name='$1', expiration='$2'},
		Guard = {'<', '$2', timestamp(now())},
		Result = '$1',
		Logins = mnesia:select(?LOGINS_TABLE,[{MatchHead, [Guard], [Result]}]),
		[mnesia:delete({?LOGINS_TABLE, Login}) || Login <- Logins]
	end)).

createLogin(LoginName) ->
	#login{name=LoginName, expiration=timestamp(now())+30}.

timestamp({Megasecs, Secs, _}) ->
     (Megasecs * 1000000) + Secs.

transac_return({atomic, Result}) ->
	{ok, Result};
transac_return({aborted, Reason}) ->
	{error, Reason}.
