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
-export([start_link/0,
         spawn_worker/1,
         spawn_worker_with_redis/0,
         fetch/0]).

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
    worker_num = 0 :: integer()
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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

spawn_worker(Wid) ->
    gen_server:call(?SERVER, {spawn_worker, Wid}).

spawn_worker_with_redis() ->
    [{client, C}] = ets:lookup(eredis, client),

    Wid =
    case set_worker_id(C, ?TRY_COUNT) of
        {error, Reason} ->
            exit(Reason);
        Id ->
            Id
    end,

    gen_server:call(?SERVER, {spawn_worker, Wid}).

fetch() ->
    gen_server:call(?SERVER, fetch).

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
init([]) ->
    {ok, #state{}}.

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
handle_call({spawn_worker, Wid}, _From, State = #state{workers = Wrks,
                                                       worker_num = Num}) ->
    {ok, Wrk} = supervisor:start_child('esnowflake_worker_sup', [Wid]),
    NewWrks = maps:put(Wid, Wrk, Wrks),
    {reply, Wrk, State#state{workers = NewWrks, worker_num = Num+1}};
handle_call(fetch, _From, State = #state{worker_num = Num, workers = Wrks}) ->
    Wid = erlang:system_time(nano_seconds) rem Num,
    {ok, Wrk} = maps:find(Wid, Wrks),
    {reply, Wrk, State}.

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
set_worker_id(_, 0) ->
    {error, over_retry_count_set_worker_id};
set_worker_id(C, Try) ->
    Key = rand:uniform(?WORKER_ID_RANGE),
    case eredis:q(C, ["SET", Key, 1, "NX"]) of
        {ok, <<"OK">>} ->
            Key;
        Any ->
            error_logger:error_msg("set worker id failed: ~p", [Any]),
            set_worker_id(C, Try-1)
    end.
