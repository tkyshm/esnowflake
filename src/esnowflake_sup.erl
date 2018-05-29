%%%-------------------------------------------------------------------
%% @doc esnowflake top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(esnowflake_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Version, Wnum) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Version, Wnum]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Version, Wnum]) ->
    SupFlags = #{
      strategy  => rest_for_one,
      intensity => 1000,
      period    => 3600
     },

    PoolSpec = #{
      id       => 'esnowflake_worker_pool',
      start    => {'esnowflake_worker_pool', start_link, []},
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

    {ok, {SupFlags, [WrkSupSpec, PoolSpec, StatsSpec]}}.
