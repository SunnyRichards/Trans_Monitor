%%%-------------------------------------------------------------------
%%% @author sunnyrichards
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Apr 2018 7:24 PM
%%%-------------------------------------------------------------------
-module(trans_mongodb).
-author("sunnyrichards").

-behaviour(gen_server).

%% API
-export([start_link/2,find_devices/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {topology :: pid()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(find_devices() -> {ok, List::list()} | {error, string() | [string()]}).
find_devices() ->
  gen_server:call(?SERVER, {find_devices}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(DbName :: atom(), WorkerOptions :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link(mongodb, WorkerOptions) ->
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

init([WorkerOptions]) ->
  io:format("Mongo DB connect '~w'", [WorkerOptions]),
  {ok, Topology} = mc_worker_api:connect(WorkerOptions),
  State = #state{topology = Topology},
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

handle_call({find_devices}, _From, State) ->
  {reply, find_devices(State), State};

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

find_devices(#state{topology = Topology}) ->
  Key = #{<<"tripStatus">> => true},
  Projector = #{projector => #{<<"_id">> => false,<<"detailsDeviceID.deviceID">> => true}},
  {ok, Cur2} = mc_worker_api:find(Topology, <<"order">>,Key,Projector),
  List = mc_cursor:rest(Cur2),
  filter_devices(List,[]).

filter_devices([],Acc) -> Acc;

filter_devices([#{<<"detailsDeviceID">> := #{<<"deviceID">> := DeviceId}}|Tail],Acc) ->
  filter_devices(Tail,[round(DeviceId)|Acc]);

filter_devices([#{}|Tail],Acc) ->
  filter_devices(Tail,Acc).
