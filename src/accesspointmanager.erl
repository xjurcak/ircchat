%%%-------------------------------------------------------------------
%%% @author xjurcak
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2014 7:21 PM
%%%-------------------------------------------------------------------
-module(accesspointmanager).
-author("xjurcak").

%% API
-export([start_link/0]).

start_link() ->
  accesspointmanager_sup:start_link(10).
