% %%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2017, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2017-10-27 01:09:21.536013
%%%-------------------------------------------------------------------
-module(esnowflake_worker_pool).

-behaviour(gen_server).

%% API
-export([start_link/1,
         spawn_worker/1,
         spawn_worker_with_redis/0,
         remove_worker/1,
         fetch/0,
         worker_ids/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("esnowflake.hrl").

-define(SERVER, ?MODULE).
-define(TRY_COUNT, 3).

-record(state, {
    workers = maps:new() :: map(),
    worker_num = 0 :: integer(),
    is_redis = false
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(IsRedis) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [IsRedis], []).

spawn_worker(Wid) ->
    gen_server:call(?SERVER, {spawn_worker, Wid}).

spawn_worker_with_redis() ->
    case get_next_worker_id(?TRY_COUNT) of
        {error, Reason} ->
            error_logger:error_msg("failed spawn worker: ~p", [Reason]);
        Wid ->
            gen_server:call(?SERVER, {spawn_worker, Wid})
    end.

remove_worker(Wid) ->
    gen_server:call(?SERVER, {remove_worker, Wid}).


fetch() ->
    gen_server:call(?SERVER, fetch).

worker_ids() ->
    gen_server:call(?SERVER, worker_ids).

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
init([IsRedis]) ->
    ets:new(?ETS_WORKER, [named_table, protected]),
    {ok, #state{is_redis = IsRedis}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({spawn_worker, Wid}, _From, State = #state{is_redis = IsRedis}) ->
    {ok, Wrk} = supervisor:start_child('esnowflake_worker_sup', [Wid, IsRedis]),
    true = ets:insert(?ETS_WORKER, {Wid, Wrk}),
    {reply, Wrk, State};
handle_call(fetch, _From, State) ->
    Wrks = ets:tab2list(?ETS_WORKER),
    N = (erlang:system_time(nano_seconds) rem length(Wrks)) + 1,
    {_, Wrk} = lists:nth(N, Wrks),
    {reply, Wrk, State};
handle_call({remove_worker, Wid}, _From, State) ->
    ets:delete(?ETS_WORKER, Wid),
    {reply, ok, State};
handle_call(worker_ids, _From, State) ->
    Wrks = ets:foldl(fun({Wid, _}, Acc) -> [Wid|Acc] end, [], ?ETS_WORKER),
    {reply, Wrks, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
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
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% private functions
get_next_worker_id(0) ->
    error_logger:error_msg("over retry count at ~p:set_worker_id/1", [?MODULE]),
    {error, over_retry_count_set_worker_id};
get_next_worker_id(Try) ->
    case esnowflake_redis:get_wid() of
        {error, Reason} ->
            {error, Reason};
        Wid ->
            case esnowflake_redis:setnx_wid(Wid) of
                {ok, <<"OK">>} ->
                    Wid;
                {ok, undefined} ->
                    {error, could_not_set_worker_id};
                Any ->
                    error_logger:error_msg("failed setnx wid: ~p. try=~p", [Any, Try]),
                    timer:sleep(500),
                    get_next_worker_id(Try-1)
            end
    end.
