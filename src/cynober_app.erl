-module(cynober_app).

-behaviour(application).

%% Aest headers

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Application callbacks
-export([start/2, stop/1, average_access_time/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    cynober_sup:start_link().

stop(_State) ->
    ok.

average_access_time(Site) ->
    {ok, PartialTimes} = estimator:check(Site),
    Sum = lists:sum(PartialTimes),
    AvgTime = Sum / length(PartialTimes),
    {average_access_time, AvgTime}.

-ifdef(TEST).

app_test() ->
    _ = cynober_app:start([],[]).

avg_test() ->
    estimator:start_link(),
    {average_access_time, _} = cynober_app:average_access_time("www.google.pl").

stop_test() ->
    ok = cynober_app:stop([]).

-endif.
