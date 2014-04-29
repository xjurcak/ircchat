-module(login_server).

-behaviour(gen_server).
-behaviour(backable).
-behaviour(starter).

%% API
-export([start/0, login/1, login/2, logout/1, touch/1, is_logged/1]).

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
-define(EXPIRATION, 30).

-include("login_server.hrl").
-include("messages.hrl").

-record(state, {}).
-record(login, {name, expiration, listener}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
	backable:backup(node(),  ?MODULE).

login(LoginName) ->
  login(LoginName, nil).
login(LoginName, Listener) ->
  gen_server:call({global, ?LOGIN_SERVER_GLOBAL}, {login, LoginName, Listener}).

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
    mnesia:create_table(?LOGINS_TABLE, [{attributes, record_info(fields,login)}]).
	 

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
  {reply, is_logged_i(LoginName), State};
handle_call({touch, LoginName}, _From, State = #state{}) ->
  {reply, touch_i(LoginName), State};
handle_call({logout, LoginName}, _From, State = #state{}) ->
  {reply, logout_i(LoginName), State};
handle_call({login, LoginName, Listener}, _From, State = #state{}) ->
  {reply, login_i(LoginName, Listener), State};
handle_call(_, _From, State = #state{}) ->
  {reply,  #message_error{reason=undef_method}, State}.

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
	transac_return(mnesia:transaction(fun() ->
	  case mnesia:read(?LOGINS_TABLE, LoginName) of
	    [] ->
		  not_logged;
	    [Login_o]->
		  Login = createLogin(Login_o#login.name, Login_o#login.listener),
		  mnesia:write(Login),
		  touched
	  end
	end)).

login_i(LoginName, Listener) ->
	Login = createLogin(LoginName, Listener),
	transac_return(mnesia:transaction(fun() ->
	  case mnesia:read(?LOGINS_TABLE, LoginName) of
	    [] ->
          io:format("~p~n",[mnesia:table_info(login, all)]),
		  mnesia:write(Login),
		  logged;
	    _->
	      already_logged
	  end
	end)).

logout_i(Login) ->
	mnesia:dirty_delete(?LOGINS_TABLE, Login#login.name),
	notify_logged_out(Login, requested),
	#message_ok{}.

clean_loop() ->
	clean(),
	timer:sleep(5000),
	clean_loop().

clean() ->
	transac_return(mnesia:transaction(fun() ->
		MatchHead = #login{name='$1', expiration='$2', _='_'},
		Guard = {'<', '$2', timestamp(now())},
		Result = '$1',
		Logins = mnesia:select(?LOGINS_TABLE,[{MatchHead, [Guard], [Result]}]),
		%io:format("~p~n", [[Logins, MatchHead, Guard, Result]]),
		[expire(Login) || Login <- Logins]
	end)).

%transac!!!
expire(LoginName) ->
	Login = mnesia:read(?LOGINS_TABLE, LoginName),
	mnesia:delete({?LOGINS_TABLE, LoginName}),
	notify_logged_out(Login, expired).

notify_logged_out(Login, Reason) ->
	if 
	  Login#login.listener =/= nil ->
		gen_server:cast(Login#login.listener, {logged_out, Login#login.name, Reason});
	  true ->
		nil
	end.

createLogin(LoginName, Listener) ->
	#login{name=LoginName, expiration=timestamp(now())+?EXPIRATION, listener=Listener}.

timestamp({Megasecs, Secs, _}) ->
     (Megasecs * 1000000) + Secs.

transac_return({atomic, Result}) ->
	#message_ok{result=Result};
transac_return({aborted, Reason}) ->
	#message_error{reason=Reason}.
