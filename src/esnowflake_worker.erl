%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2017, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2017-10-26 22:20:17.303766
%%%-------------------------------------------------------------------
-module(esnowflake_worker).

-behaviour(gen_server).

-include("esnowflake.hrl").

%% API
-export([start_link/2,
        generate_id/1,
        generate_ids/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(REFETCH_PERIOD(N), N*1000).

-record(state, {
    worker_id      = 0 :: integer(), % 0-1023
    sequence       = 0 :: integer(), % 0-4095
    pre_timestamp  = erlang:system_time(milli_seconds) :: non_neg_integer(),
    refetch_period = 1 :: non_neg_integer() % refetch worker id period (seconds)
}).

-type worker_state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Id) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Id, IsRedis) ->
    gen_server:start_link(?MODULE, [Id, IsRedis], []).

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
init([Id, true]) ->
    RP = application:get_env(esnowflake, refetch_period, 1),
    erlang:send_after(?REFETCH_PERIOD(RP), self(), refetch_worker_id),
    {ok, #state{worker_id = Id, refetch_period = RP}};
init([Id, false]) ->
    {ok, #state{worker_id = Id}}.

-spec generate_id(pid()) -> integer() |
                            {err_clock_backwards, term()}.
generate_id(Pid) ->
    gen_server:call(Pid, {generate, erlang:system_time(milli_seconds)}).

-spec generate_ids(pid(), integer()) -> [integer()] |
                                        {err_clock_backwards, term()}.
generate_ids(Pid, Num) ->
    gen_server:call(Pid, {multi_generate, erlang:system_time(milli_seconds), Num}).

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
handle_call({generate, Now}, _From, State) ->
    case snowflake_id(Now, State) of
        {Id, NewState} ->
            {reply, Id, NewState};
        {error, Reason, Msg, NewState} ->
            {reply, {Reason, Msg}, NewState}
    end;
handle_call({multi_generate, Now, Num}, _From, State) ->
    case snowflake_ids(Now, Num, State) of
        {Ids, NewState} ->
            {reply, Ids, NewState};
        {error, Reason, Msg, NewState} ->
            {reply, {Reason, Msg}, NewState}
    end.

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
handle_cast(_Req, State) ->
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
handle_info(refetch_worker_id, State = #state{worker_id = Wid, refetch_period = RP}) ->
    case esnowflake_redis:setxx_wid(Wid) of
        {ok, <<"OK">>} ->
            erlang:send_after(?REFETCH_PERIOD(RP), self(), refetch_worker_id),
            {noreply, State};
        {ok, undefined} ->
            esnowflake_worker_pool:spawn_worker_with_redis(),
            {stop, {shutdown, already_assigned_worker_id}, State};
        {error, Reason} ->
            error_logger:error_msg("refetch_worker_id failed: id=~p reason=~p~n", [Wid, Reason]),
            erlang:send_after(?REFETCH_PERIOD(RP), self(), refetch_worker_id),
            {noreply, State}
    end;
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
terminate({shutdown, _Reason}, #state{worker_id = Wid}) ->
    esnowflake_worker_pool:remove_worker(Wid),
    ok;
terminate(_Reason, #state{worker_id = Wid}) ->
    esnowflake_worker_pool:remove_worker(Wid),
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

%% @private
%% @doc
%% generate 64bit snowflake id
%% @end
-spec snowflake_id(Now :: integer(), State :: worker_state()) ->
    integer() |
    {error, err_clock_backwards, term(), worker_state()}.
snowflake_id(Now, State = #state{pre_timestamp = PreTS}) when Now < PreTS ->
    Msg = io_lib:format("timestamp diff: ~p", [PreTS - Now]),
    {error, err_clock_backwards, Msg, State};
snowflake_id(Now, State = #state{pre_timestamp = PreTS,
                                 sequence = Seq,
                                 worker_id = Wid}) when Now == PreTS ->
    % if sequnece number was exhausted, foward 1 ms.
    {NewSeq, NewNow} = next_sequence(<<Seq:12>>, Now),
    Id = generate(NewNow , Wid, NewSeq),
    {Id, State#state{pre_timestamp = NewNow, sequence = NewSeq}};
snowflake_id(Now, State = #state{worker_id = Wid}) ->
    {generate(Now, Wid, 0), State#state{pre_timestamp = Now}}.

%% @private
%% @doc
%% generate 64bit snowflake ids
%% @end
-spec snowflake_ids(Now :: integer(), Num :: integer(), State :: worker_state()) ->
    [integer()] |
    {error, err_clock_backwards, term(), worker_state()}.
snowflake_ids(Now, Num, State) ->
    snowflake_ids(Now, Num, [], State).

snowflake_ids(_Now, Num, Ids, State) when length(Ids) =:= Num ->
    {Ids, State};
snowflake_ids(Now, Num, Ids, State) ->
    case snowflake_id(Now, State) of
        {Id, NewState} ->
            NewNow = erlang:system_time(milli_seconds),
            snowflake_ids(NewNow, Num, [Id|Ids], NewState);
        {error, Reason, Msg, NewState} ->
            {error, Reason, Msg, NewState}
    end.

%% @private
%% @doc
%% generate 64bit uniq id by snowflake algorithm
%% @end
-spec generate(TS :: integer(), Wid :: integer(), Seq :: integer()) -> integer().
generate(Now, Wid, Seq) ->
    TS = Now - ?TWEPOCH,
    <<GenId:64/integer>> = <<0:1, TS:41/integer, Wid:10/integer, Seq:12/integer>>,
    GenId.

%% @private
%% @doc
%% Calculates the next sequential number between 0 to 4095 (12bit).
%% If sequencial number was exhausted, sleep 1 ms and return 0 sequence number.
%% @end
-spec next_sequence(<<_:12>>, Now :: integer()) -> {Seq :: non_neg_integer(), NewNow :: integer()}.
next_sequence(<<2#111111111111:12>>, _Now) ->
    timer:sleep(1),
    {0, erlang:system_time(milli_seconds)};
next_sequence(<<Seq:12>>, Now) ->
    {Seq + 1, Now}.
