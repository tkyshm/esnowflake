%% @doc esnowflake application API
%% @end

-module(esnowflake_app).

-behaviour(application).

-include("esnowflake.hrl").

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, StartArgs) ->
    [MinId, MaxId] = application:get_env(esnowflake, worker_min_max_id, ?DEFAULT_WORKER_MIN_MAX),

    Version = proplists:get_value(vsn, StartArgs),

    {Redis, WorkerNum} =
    case application:get_env(esnowflake, redis, undefined) of
        undefined ->
            % do nothing
            {undefined, MaxId-MinId+1};
        Args ->
            {ok, C} = eredis:start_link(Args),
            Wnum = application:get_env(esnowflake, worker_num, ?DEFAULT_WORKER_NUM),
            {C, Wnum}
    end,

    {ok, Pid} = esnowflake_sup:start_link(Version, Redis),

    case Redis of
       undefined ->
           start_workers(MinId, MaxId);
       _ ->
           start_workers(WorkerNum)
    end,

    {ok, Pid}.

stop(_State) ->
    ok.

start_workers(0) ->
    ok;
start_workers(WorkerNum) ->
    esnowflake_worker_pool:spawn_worker_with_redis(),
    start_workers(WorkerNum-1).

start_workers(MinId, MaxId) ->
    [esnowflake_worker_pool:spawn_worker(Wid) || Wid <- lists:seq(MinId, MaxId)].
