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

    {ok, Pid} = esnowflake_sup:start_link(Version),
    [esnowflake_worker_pool:spawn_worker(Wid) || Wid <- lists:seq(MinId, MaxId)],

    {ok, Pid}.

stop(_State) ->
    ok.
