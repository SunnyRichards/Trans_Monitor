%%%-------------------------------------------------------------------
%%% @author sunnyrichards
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Apr 2018 7:34 PM
%%%-------------------------------------------------------------------
-module(trans_email_handler).
-author("sunnyrichards").

-export([do_send_alert/1]).

-define(NAME, "email").
-define(RELAY, "smtp.gmail.com").
-define(USERNAME, "yourmail.com").
-define(PASSWORD, "yourpassword").


%%%===================================================================
%%% API
%%%===================================================================

-spec(do_send_alert(Data :: list()) -> {ok, pid()}).

do_send_alert([]) ->
  gen_smtp_client:send({?USERNAME,
    [receiver@gmail.com],
    "Subject: SUCCESS !!! All Devices are Responding !!! \r\nFrom: Transecur - [DevTeam] \r\nTo: Admin \r\n\r\n No device is in faulty state, all are working fine. \n Cheers"},
    [{relay, ?RELAY}, {username, ?USERNAME},{password, ?PASSWORD},
      {ssl, false},{tls, always},{no_mx_lookups, true},{auth, always}]);


do_send_alert(Data) ->
  gen_smtp_client:send({?USERNAME,
    ["sunny@lynkit.in","manas@lynkit.in"],
      "Subject: ALERT !!! - Devices not Responding !!! \r\nFrom: Transecur - [DevTeam] \r\nTo: Admin \r\n\r\n List of Devices: " ++  Data},
    [{relay, ?RELAY}, {username, ?USERNAME},{password, ?PASSWORD},
      {ssl, false},{tls, always},{no_mx_lookups, true},{auth, always}]).


