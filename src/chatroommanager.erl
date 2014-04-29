%%%-------------------------------------------------------------------
%%% @author xjurcak
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2014 7:21 PM
%%%-------------------------------------------------------------------
-module(chatroommanager).
-author("xjurcak").

-include("messages.hrl").


%% API
-export([start_link/0, get_chat_room/2, nodes_from_netnodes/2, create_room/2]).

start_link() ->
  chatroommanager_sup:start_link(10).

create_room([ChatManager | T], ChatName) ->
  case create_room(ChatManager, ChatName) of
    #message_ok{} = Message ->
      io:format("create room ~p~n", [Message]),
      Message;
    _ ->
      io:format("create room error"),
      create_room(T, ChatName)
  end;

create_room([], _ChatName) ->
  nochatroom;

create_room(Node = #netnode{}, Name) ->
  io:format("create room"),
  chatroommanager_serv:create_room(Node, Name).


get_chat_room(ChatroomsManagers, Name) ->
  Nodes = nodes_from_netnodes(ChatroomsManagers, []),
  {Replies, _Bad} = chatroommanager_serv:get_room_multicall(Nodes, Name),
  find_chatroom_in_replie(Replies).


nodes_from_netnodes([#netnode{ node = Node} | T], Nodes) ->
  nodes_from_netnodes(T, [Node|Nodes]);

nodes_from_netnodes([], Nodes) ->
  Nodes.

find_chatroom_in_replie([{_Node, #message_ok{result = Pid}} | _T]) ->
  {ok, Pid};

find_chatroom_in_replie([{_Node, _Reply} | T ]) ->
  find_chatroom_in_replie(T);

find_chatroom_in_replie( []) ->
  nofind.

