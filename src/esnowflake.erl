%%% @doc Generates uniq id API which inspires Twiiter's snowflake.
%%%
%%% @end
-module(esnowflake).

-include("esnowflake.hrl").

-export([generate_id/0,
         generate_ids/1,
         to_unixtime/1,
         to_unixtime/2,
         decode_id/1,
         stats/0]).

-define(RECEIVE_TRIES, 3).
-define(RECEIVE_TIMEOUT, 3000).

-type snowflake_id() :: integer().
-type time_unit() :: seconds | milli_seconds.

%% @doc
%% Generate a uniq id.
%% @end
-spec generate_id() -> snowflake_id().
generate_id() ->
    Wrk = esnowflake_worker_pool:fetch(),
    esnowflake_worker:generate_id(Wrk).

%% @doc
%% Generate uniq ids, but one worker generates theses ids.
%% @end
-spec generate_ids(integer()) -> [snowflake_id()].
generate_ids(Num) ->
    Wrk = esnowflake_worker_pool:fetch(),
    esnowflake_worker:generate_ids(Wrk, Num).

%% @doc
%% Convert generated id to unix time (milli_seconds).
%% @end
-spec to_unixtime(snowflake_id()) -> integer().
to_unixtime(Id) ->
    to_unixtime(Id, milli_seconds).

%% @doc
%% Convert generated id to unix time (seconds or milli_seconds).
%% The actual unix time (msec) is round down to the nearest decimal if you specified 'seconds' time unit.
%% @end
-spec to_unixtime(snowflake_id(), time_unit()) -> integer().
to_unixtime(Id, Unit) ->
    <<0:1, TS:41, _:22>> = <<Id:64>>,
    case Unit of
        seconds ->
            (TS + ?TWEPOCH) div 1000;
        milli_seconds ->
            TS + ?TWEPOCH
    end.

%% @doc
%% Decode generated id to unix time, machine id and sequential number.
%% Unit of unix time is milli_seconds.
%% Machine id is same as esnowflake application's  worker id.
%% Sequential number is index of same timestamp and worker id,
%% @end
-spec decode_id(snowflake_id()) -> {Timestamp :: integer(),
                                    MachineID :: integer(),
                                    Sequence  :: integer()}.
decode_id(Id) ->
    <<0:1, Timestamp:41, MachineID:10, Sequence:12>> = <<Id:64>>,
    {Timestamp, MachineID, Sequence}.

%% @doc
%% esnowflake stats
%% @end
-spec stats() -> term().
stats() ->
    esnowflake_stats:stats().
