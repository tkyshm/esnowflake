%%%-------------------------------------------------------------------
%% @doc esnowflake top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(esnowflake_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Version, Wnum, Redis) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Version, Wnum, Redis]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Version, Wnum, Redis]) ->
    SupFlags = #{
      strategy  => rest_for_one,
      intensity => 1000,
      period    => 3600
     },

    {IsRedis, RedisClientSpec} =
    case Redis of
        undefined ->
            {false, undefined};
        _ ->
            Spec = #{
              id       => 'esnowflake_redis',
              start    => {'esnowflake_redis', start_link, [Redis]},
              restart  => permanent,
              shutdown => 2000,
              type     => worker,
              modules  => ['esnowflake_worker_pool']
             },

            {true, Spec}
    end,

    PoolSpec = #{
      id       => 'esnowflake_worker_pool',
      start    => {'esnowflake_worker_pool', start_link, [IsRedis]},
      restart  => permanent,
      shutdown => 2000,
      type     => worker,
      modules  => ['esnowflake_worker_pool']
     },

    WrkSupSpec = #{
      id       => 'esnowflake_worker_sup',
      start    => {'esnowflake_worker_sup', start_link, []},
      restart  => permanent,
      shutdown => 2000,
      type     => supervisor,
      modules  => ['esnowflake_worker_sup']
     },

    StatsSpec = #{
      id       => 'esnowflake_stats',
      start    => {'esnowflake_stats', start_link, [Version, Wnum]},
      restart  => permanent,
      shutdown => 2000,
      type     => worker,
      modules  => ['esnowflake_stats']
     },

    Specs =
    [ S ||
      S <- [RedisClientSpec, WrkSupSpec, PoolSpec, StatsSpec],
      S =/= undefined],

    {ok, {SupFlags, Specs}}.
