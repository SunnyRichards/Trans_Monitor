%%%-------------------------------------------------------------------
%%% @author sunnyrichards
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Apr 2018 7:29 PM
%%%-------------------------------------------------------------------
  -module(trans_redis).
-author("sunnyrichards").

-behaviour(gen_server).

%% API
-export([start_link/2,check_db/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {connection :: pid()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(check_db() -> ok).

check_db() ->
  gen_server:call(?SERVER, {check_db}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(DBName :: atom(), WorkerOptions :: list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link(redis, WorkerOptions) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [WorkerOptions], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([[Host,Port]] = WorkerOptions) ->
 io:format("Redis DB connect '~w'", [WorkerOptions]),
  {ok, Connection} = eredis:start_link(Host,Port),
  State = #state{connection = Connection},
  timer:apply_interval(7200000, ?MODULE, check_db, []),
  {ok, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({check_db}, _From, State) ->
  {reply, check_db(State), State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).

terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_db(#state{connection = Pid}) ->
  Devices = trans_mongodb:find_devices(),
  io:format("Received all Active Devices: ~p~n",[Devices]),
  case fetch_packet_time(Pid,Devices,[]) of
    [] ->
      trans_email_handler:do_send_alert([]);
    DeviceIds ->
      io:format("Received Devices: ~p~n",[binary_to_list(DeviceIds)]),
      trans_email_handler:do_send_alert(binary_to_list(DeviceIds)),
      trans_sms_handler:do_send_alert(binary_to_list(DeviceIds))
  end.

%%%===================================================================
%%% Internal functions - CronJob Utility
%%%===================================================================

fetch_packet_time(_Pid,[],Acc) ->
  list_to_binary(Acc);

fetch_packet_time(Pid,[Key|Others],Acc = []) ->
  case eredis:q(Pid, ["HGET", Key, packet_time]) of
    {ok,undefined} ->
      fetch_packet_time(Pid,Others,[integer_to_list(Key)|Acc]);
    {ok,PacketTime} ->
      case (helper_time:timestamp() - 7200) < binary_to_integer(PacketTime) of
        true ->
          fetch_packet_time(Pid,Others,Acc);
        false ->
          fetch_packet_time(Pid,Others,[integer_to_list(Key)|Acc])
      end;
    _Error ->
      fetch_packet_time(Pid,Others,[Key|Acc])
  end;

fetch_packet_time(Pid,[Key|Others],Acc) ->
  case eredis:q(Pid, ["HGET", Key, packet_time]) of
    {ok,undefined} ->
      fetch_packet_time(Pid,Others,[integer_to_list(Key),","|Acc]);
    {ok,PacketTime} ->
      case (helper_time:timestamp() - 7200) < binary_to_integer(PacketTime) of
        true ->
          fetch_packet_time(Pid,Others,Acc);
        false ->
          fetch_packet_time(Pid,Others,[integer_to_list(Key),","|Acc])
      end;
    _Error ->
      fetch_packet_time(Pid,Others,[Key|Acc])
  end.
