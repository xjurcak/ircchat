%%%-------------------------------------------------------------------
%%% @author xjurcak
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2014 10:19 AM
%%%-------------------------------------------------------------------
-author("xjurcak").


-record(chat_message, {message, timestamp, from}).

-record(message_ok, { result = nil :: any() }).
-record(message_error, { reason :: atom(), reason_message = '' :: term() }).

-type message_ok() :: #message_ok{}.
-type message_error() :: #message_error{}.

-record(netnode, { node :: atom(), name :: atom()}).

-type netnode() :: #netnode{}.