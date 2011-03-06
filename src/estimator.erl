-module(estimator).

-behaviour(gen_server).

-export([start_link/0, check/1, check/2, check/3, get_site/1, spawn_worker/3, collect_work/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TIMEOUT, 10000).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, 0, ?TIMEOUT}.

check(Site) ->
    gen_server:call(?MODULE, {site_check, Site, 10, 10}).
check(Site,  NumberOfConcurrentProcesses) ->
    gen_server:call(?MODULE, {site_check, Site,  NumberOfConcurrentProcesses, 10}).
check(Site,  NumberOfConcurrentProcesses, PullsPerProcess) ->
    gen_server:call(?MODULE, {site_check, Site,  NumberOfConcurrentProcesses, PullsPerProcess}).




%% callbacks

handle_call({site_check, Site, NumberOfConcurrentProcesses, PullsPerProcess }, _From, List) ->
    Parent = self(),
    for(1, NumberOfConcurrentProcesses, fun(_) ->  spawn(fun() -> estimator:spawn_worker(Parent, Site, PullsPerProcess) end) end),
    Response = for(1, NumberOfConcurrentProcesses, fun(_) -> estimator:collect_work() end),
    {reply, {ok , Response}, List};

% don't break compilation!

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% utils

%% for
for(Max, Max, Foo) -> [Foo(Max)];
for(I, Max, Foo) -> [Foo(I) | for(I+1, Max, Foo)].

%% get site

get_site(Site) ->
    {ok, Socket} = gen_tcp:connect(Site, 80, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
    receive_site_data(Socket, []).

receive_site_data(Socket, SoFar) ->
    receive
      {tcp, Socket, Bin} ->
            receive_site_data(Socket, [Bin | SoFar]);
      {tcp_closed, Socket} -> binary:list_to_bin(lists:reverse(SoFar))
    end.

spawn_worker(ParentProc, Site, PullsPerProcess) ->
    { _ , BeforeSeconds, BeforeMicroseconds} = erlang:now(),
    for(1, PullsPerProcess, fun(_) -> estimator:get_site(Site) end),
    { _ , Seconds, Microseconds} = erlang:now(),
    ExecutionTime = (Seconds  + (Microseconds / 1000000)) - (BeforeSeconds  + (BeforeMicroseconds / 1000000) ),
    ParentProc ! {worker_result, ExecutionTime }.

collect_work() ->
    receive
        {worker_result, TimeSpentOnWork} -> TimeSpentOnWork
    end.

-define(TEST_SITE, "www.google.com").

-ifdef(TEST).

prepare() ->
    estimator:start_link().

for_test() ->
    [1,2,3,4,5] = for(1,5, fun(X) -> X end).

estimator_test() ->
    prepare(),
    { ok, _ } = estimator:check(?TEST_SITE).

worker_test() ->
    prepare(),
    spawn_worker(self(), ?TEST_SITE, 2),
    _ = collect_work().


gen_server_generic_method_test() ->
    gen_server:cast(?MODULE, {message, from, test}).

-endif.
