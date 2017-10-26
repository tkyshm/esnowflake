%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2017, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2017-10-27 02:49:54.099694
%%%-------------------------------------------------------------------
-module(esnowflake_stats).

-behaviour(gen_server).

%% API
-export([start_link/2,
         stats/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    version = undefined :: list() | undefined,
    worker_num = 0 :: integer(),
    pre_wall_time = lists:sort(erlang:statistics(scheduler_wall_time))
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Version, WorkerNum) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Version, WorkerNum) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Version, WorkerNum], []).

stats() ->
    gen_server:call(?SERVER, stats).

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
init([Version, WorkerNum]) ->
    erlang:system_flag(scheduler_wall_time, true),
    {ok, #state{version = Version, worker_num = WorkerNum}}.

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
handle_call(stats, _From, State = #state{pre_wall_time = Ts0}) ->
    Ts1 = lists:sort(erlang:statistics(scheduler_wall_time)),
    DiffWallTime = lists:zip(Ts0, Ts1),

    Cpus = [{I, (A1 - A0)/(T1 - T0)} || {{I, A0, T0}, {I, A1, T1}} <- DiffWallTime],

    {A, T} =
    lists:foldl(fun({{_, A0, T0}, {_, A1, T1}}, {Ai,Ti}) ->
                        {Ai + (A1 - A0), Ti + (T1 - T0)}
                end, {0, 0}, DiffWallTime),
    TotalCpuUsage = A / T,

    Stats = [{cpu_usage, Cpus},
             {total_cpu_usage, TotalCpuUsage},
             {memory, erlang:memory()}],

    {reply, Stats, State#state{pre_wall_time = Ts1}}.

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

%%%===================================================================
%%% Internal functions
%%%===================================================================
