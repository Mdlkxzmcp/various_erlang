-module(counter0).
-export([start/0, loop/1, tick/1, read/0]).

%%
%% @doc start/0
%%
%% Registers the module name `counter0` globally, then spawns the `loop/1`
%% function with `0` as the initial argument.
start() ->
    register(counter0, spawn(counter0, loop, [0])).

%%
%% @doc tick/1
%%
%% Increment the counter by given `N`.
-spec tick(number()) -> {reference(), 'ack'}.
tick(N) -> rpc({tick, N}).


%%
%% @doc read/0
%%
%% Get the current state of the counter.
-spec read() -> {reference(), number()}.
read() -> rpc(read).


%%
%% @doc loop/1
%%
%% Serves as the body of the counter.
-spec loop(number()) -> no_return().
loop(State) ->
    receive
        {From, Tag, {tick, N}} ->
            From ! {Tag, ack},
            loop(State + N);
        {From, Tag, read} ->
            From ! {Tag, State},
            loop(State)
    end.


%% @doc rpc/1
-spec rpc(any()) -> any().
rpc(Query) ->
    Tag = make_ref(),
    counter0 ! {self(), Tag, Query},
    receive
        {Tag, Reply} ->
            Reply
    end.
