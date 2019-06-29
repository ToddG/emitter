-module(wavegen).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    prometheus:start(),
    register_metrics(),
    [tick(500, F, 0) || F <- [fun flatline/1, fun flipflop/1, fun sinewave/1, fun incrementer/1]],
    %% this never get's called
    receive
        {ok} -> ok
    end,
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
%%
%%--------------------------------------------------------------------
%% register metrics with prometheus library
%% this must be done prior to using metrics
%%--------------------------------------------------------------------
register_metrics() ->
    prometheus_counter:new([{name, mycounter}, {help, "count the times each method has been invoked"}, {labels, [flatline, incrementer, flipflop, sinewave]}]),
    prometheus_gauge:new([{name, myguage}, {help, "current value of a method"}, {labels, [flatline, incrementer, flipflop, sinewave]}]),
    prometheus_summary:new([{name, mysummary}, {help, "summary of a method"}, {labels, [flatline, incrementer, flipflop, sinewave]}]),
    prometheus_histogram:new([{name, myhist}, {help, "hist of a method"}, {labels, [method]},
                              {buckets, [10, 100, 1000, 100000, 1000000]}]),
    ok.

%%--------------------------------------------------------------------
%% multimetric : emit all the metrics since we're just trying to
%% sort out how all this works. this way we can see stuff in the
%% grafana dashboard
%%--------------------------------------------------------------------
multimetric(Label, Value) ->
    prometheus_counter:inc(mycounter, [Label], Value),
    prometheus_gauge:set(myguage, [Label], Value),
    prometheus_summary:observe(mysummary, [Label], Value),
    prometheus_histogram:observe(myhist, [Label], Value).

%%--------------------------------------------------------------------
%% emit : side-effect for the function F
%%--------------------------------------------------------------------
emit(Name, Value) -> 
    io:format("emit: [~p]~p~n", [Name, Value]),
    multimetric(Name, Value).


%%--------------------------------------------------------------------
%% flat line : emits the value 1 for a flat line shape
%%--------------------------------------------------------------------
flatline(_) -> emit("flatline", 1), ok.

%%--------------------------------------------------------------------
%% incrementer : always increments value
%%--------------------------------------------------------------------
incrementer(V) -> 
    V1 = V + 1,
    emit("incrementer", V1),
    V1.

%%--------------------------------------------------------------------
%% flip flop : emits the value 1 and then the value 0
%%--------------------------------------------------------------------
flipflop(0) -> emit("flipflop", 1), 1;
flipflop(1) -> emit("flipflop", 0), 0;
flipflop(_) -> emit("flipflop", 0), 0.

%%--------------------------------------------------------------------
%% sine wave
%%--------------------------------------------------------------------
sinewave(V) ->
    sinewave(V, 1).
sinewave(V, Delta) ->
    emit("sinewave", math:sin(V)),
    V1 = V + Delta,
    V1.

%%--------------------------------------------------------------------
%% tick calls itself every N milliseconds and invokes function F on 
%% Data D; returns a cancel function C
%%--------------------------------------------------------------------
tick(N, F, D) ->
    Pid = spawn(fun() -> timer(N, F, D) end),
    fun() -> Pid ! cancel end.


%%--------------------------------------------------------------------
%% timer calls itself every Delay milliseconds and invokes function F on 
%% Data D
%%--------------------------------------------------------------------
timer(Delay, F, D) ->
    receive cancel  -> 
                void
    after Delay     -> 
              D1 = F(D),
              timer(Delay, F, D1)
    end.


