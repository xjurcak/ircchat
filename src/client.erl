%%%-------------------------------------------------------------------
%%% @author xjurcak
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Apr 2014 2:26 PM
%%%-------------------------------------------------------------------
-module(client).
-author("xjurcak").

-include("lookup.hrl").

%% API
-export([start/0, getAccessPoint/0]).



-spec(start() ->
  ok | false).
start() ->
  ok.


-spec(getAccessPoint() ->
  ok | false).

getAccessPoint() ->
  net_kernel:connect_node(?LOOKUP_SERVER),
  lookup:get_access_point(),
  net_kernel:disconnect(?LOOKUP_SERVER).

