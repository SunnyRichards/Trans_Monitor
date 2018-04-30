%%%-------------------------------------------------------------------
%%% @author sunnyrichards
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Apr 2018 7:32 PM
%%%-------------------------------------------------------------------
-module(helper_time).
-author("sunnyrichards").

%% API
-export([timestamp/0]).

-spec(timestamp() -> integer()).
timestamp() ->
  timer:now_diff(os:timestamp(), {0, 0, 0}) div 1000000.