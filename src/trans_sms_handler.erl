%%%-------------------------------------------------------------------
%%% @author sunnyrichards
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Apr 2018 1:33 PM
%%%-------------------------------------------------------------------
-module(trans_sms_handler).
-author("sunnyrichards").

-define(DATA_TYPE, "application/json").
-define(BASE_URL, "http://enterprise.smsgupshup.com/GatewayAPI/rest").


%% API

-export([do_send_alert/1]).

%%%===================================================================
%%% API Alert
%%%===================================================================

-spec(do_send_alert(Message:: binary()) -> ok).

do_send_alert(Data) ->
  application:start(inets),
  application:start(ssl),
  Msg = "List of all Inactive Devices: " ++ Data,
  Userdata = expand_params([{"method","sendMessage"},{"send_to","9810344152"},{"msg",Msg},{"msg_type","TEXT"},
                            {"userid","2000176085"},{"auth_scheme","PLAIN"},{"password","MQ00NgZVf"},
                            {"mask","TRNSCR"},{"format","JSON"}]),
  Resp =  httpc:request(post,
    {"http://enterprise.smsgupshup.com/GatewayAPI/rest", [], "application/x-www-form-urlencoded", Userdata}, [], []),
  io:format("Response3: ~p~n",[Resp]).

expand_params(Params) ->
  ParamStrings = [edoc_lib:escape_uri(Name) ++ "=" ++ edoc_lib:escape_uri(Value)
    || {Name, Value} <- Params],
  string:join(ParamStrings, "&").