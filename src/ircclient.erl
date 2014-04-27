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

%% API
-export([get_access_point/0, start/0]).



-spec(start() ->
  ok | false).
start() ->
  ok.


-spec(get_access_point() ->
  ok | false).

get_access_point() ->
  case net_adm:ping(?LOOKUP_SERVER) of
    pang ->
      {error, nolookupserver};
    pong ->
      timer:sleep(2000),
      lookup:get_access_point()
  end.

