%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2017, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2017-10-27 00:27:41.867701
%%%-------------------------------------------------------------------
-module(esnowflake_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags = #{
      strategy  => simple_one_for_one,
      intensity => 1000,
      period    => 3600
     },

    WorkerSpec = #{
      id       => 'esnowflake_worker',
      start    => {'esnowflake_worker', start_link, []},
      restart  => permanent,
      shutdown => 2000,
      type     => worker,
      modules  => ['esnowflake_worker']
     },

    {ok, {SupFlags, [WorkerSpec]}}.
