-module(wavegen).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
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
%% emit : side-effect for the function F
%%--------------------------------------------------------------------
emit(Name, Value) -> io:format("emit: [~p]~p~n", [Name, Value]).

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


