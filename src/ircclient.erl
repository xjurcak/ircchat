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
  net_kernel:connect_node(?LOOKUP_SERVER),
  Result = lookup:get_access_point(),
  net_kernel:disconnect(?LOOKUP_SERVER),
  Result.

