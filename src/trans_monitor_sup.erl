%%%-------------------------------------------------------------------
%% @doc trans_monitor top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(trans_monitor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}

init([]) ->

  {ok, MongoDB} = application:get_env(trans_monitor, trans_monitor_monogo),
  {ok, RedisDB} = application:get_env(trans_monitor, trans_monitor_redis),
  MonGOType = proplists:get_value(type, MongoDB),
  MonGOSettings = proplists:get_value(settings, MongoDB),
  RedisType = proplists:get_value(type, RedisDB),
  RedisSettings = proplists:get_value(settings, RedisDB),

  SupFlags = #{strategy => one_for_one, intensity => 1000, period => 3600},
  ChildSpecs = [

    #{
      id => trans_mongodb,
      start => {trans_mongodb, start_link, [MonGOType, MonGOSettings]},
      restart => permanent,
      shutdown => 3000,
      type => supervisor,
      modules => dynamic
    },

    #{
      id => trans_redis,
      start => {trans_redis, start_link, [RedisType, RedisSettings]},
      restart => permanent,
      shutdown => 3000,
      type => supervisor,
      modules => dynamic
    },

    #{
      id => monitor,
      start => {monitor, start_link, []},
      restart => permanent,
      shutdown => 3000,
      type => supervisor,
      modules => dynamic
    }

  ],
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
